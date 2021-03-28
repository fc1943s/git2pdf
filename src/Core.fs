namespace Git2Pdf.Cli

open System.Diagnostics
open System.Linq
open System.Collections.Concurrent
open System
open System.Collections.Generic
open System.Text.RegularExpressions
open System.Reflection
open Argu


[<AutoOpen>]
module Core =

    [<AutoOpen>]
    module Extensions =
        type IEnumerable<'T> with
            member this.FirstOrDefault(predicate, defaultValue) =
                let value =
                    match predicate with
                    | Some predicate -> this.FirstOrDefault predicate
                    | None -> this.FirstOrDefault()

                if Object.Equals(value, Unchecked.defaultof<'T>) then
                    defaultValue ()
                else
                    value

            member this.FirstOrDefault defaultValue = this.FirstOrDefault(None, defaultValue)


        module Async =
            let map f x = async.Bind(x, async.Return << f)
            let choose fn = map (fn id)

            let throttle n fns =
                seq {
                    let n = new Threading.Semaphore(n, n)

                    for fn in fns ->
                        async {
                            do! Async.AwaitWaitHandle n |> Async.Ignore
                            let! result = Async.Catch fn
                            n.Release() |> ignore

                            return
                                result
                                |> function
                                | Choice1Of2 result -> result
                                | Choice2Of2 ex -> raise ex
                        }
                }


    module Startup =
        let parseArgsIo<'T when 'T :> IArgParserTemplate> args =
            let errorHandler =
                ProcessExiter(
                    colorizer =
                        function
                        | ErrorCode.HelpText -> None
                        | _ -> Some ConsoleColor.Red
                )

            let parser =
                ArgumentParser.Create<'T>(
                    programName =
                        Assembly.GetEntryAssembly().GetName().Name
                        + ".exe",
                    errorHandler = errorHandler
                )

            parser.ParseCommandLine args

    module Regexxer =
        let defaultOptions =
            RegexOptions.IgnoreCase ||| RegexOptions.Multiline

        // TODO: tests + idiomatic f# rewrite
        let matchAllEx (text, pattern, replace, options) =
            let options = defaultArg options defaultOptions

            let res = List<IList<string>>()
            let reg = Regex(pattern, options)
            let mutable _replacedText = ""
            let mutable _replaceOffset = 0

            let mutable _match = reg.Match text

            while _match.Success do
                let curr = List<string>()
                res.Add curr

                for i = 1 to _match.Groups.Count - 1 do
                    let group = _match.Groups.[i]

                    if group.Value = "" then
                        curr.Add ""
                    else
                        for v in group.Captures do
                            let mutable _block = v.Value

                            match replace with
                            | Some replace ->
                                _block <- replace _block

                                _replacedText <-
                                    text.Substring(_replaceOffset, v.Index - _replaceOffset)
                                    + _block

                                _replaceOffset <- v.Index + v.Length
                            | None -> ()

                            curr.Add _block

                _match <- _match.NextMatch()

            _replacedText <- _replacedText + text.Substring _replaceOffset

            {| Result = res
               ReplacedText = _replacedText |}


        let matchAll (text, pattern) =
            matchAllEx(text, pattern, None, None)
                .Result.FirstOrDefault(fun _ -> List<string>() :> IList<_>)

        let matchFirst (text, pattern) =
            matchAll(text, pattern)
                .FirstOrDefault(fun _ -> "")

        let hasMatch (text, pattern) = matchAll(text, pattern).Count > 0

    module Runtime =
        let executePowerShellAsync (cmd: string list) =
            async {
                let cmd = cmd |> String.concat "; "
                let cmd = cmd.Replace("\"", "\\\"\"")
                //  let cmd = if powershell then cmd.Replace ("\\", "\\\\") else cmd
                let startInfo =
                    ProcessStartInfo(
                        FileName = "powershell",
                        Arguments = $""" -c "%s{cmd}" """,
                        RedirectStandardOutput = true,
                        RedirectStandardError = true,
                        UseShellExecute = false,
                        CreateNoWindow = true
                    )

                use proc = new Process(StartInfo = startInfo)
                let log = ConcurrentStack()

                let event =
                    fun (error: bool) (e: DataReceivedEventArgs) ->
                        if e.Data <> null then
                            let txt =
                                (Regexxer.matchAllEx (e.Data, @"\[(={20,})", Some(fun _ -> "="), None))
                                    .ReplacedText

                            try
                                printfn $"{if error then 'E' else ' '}{proc.Id}: {txt}"
                            with ex -> eprintfn $"ERROR ON PROCESS DATA. TXT: {txt}"

                            log.Push txt

                proc.OutputDataReceived.Add(event false)
                proc.ErrorDataReceived.Add(event true)

                printfn $"Starting process: {cmd}"

                if not (proc.Start()) then
                    failwithf $"Error executing script: {cmd}"

                proc.BeginErrorReadLine()
                proc.BeginOutputReadLine()

                do! proc.WaitForExitAsync() |> Async.AwaitTask

                let result =
                    log.Reverse() |> String.concat Environment.NewLine

                printfn $"Process finished with result: {proc.ExitCode}"

                return proc.ExitCode, result
            }
