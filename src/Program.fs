namespace Git2Pdf.Cli

open Argu
open System
open System.Text
open Git2Pdf.Cli.Core
open LibGit2Sharp
open MigraDoc.DocumentObjectModel
open MigraDoc.Rendering


module Git2Pdf =
    let runCmd dir exe arguments =
        async {
            let! errorCode, result = Runtime.executeBinaryInteropAsync dir exe arguments

            return
                match errorCode with
                | 0 -> Some result
                | _ -> None
        }

    let getCommitLogs repoPath branchName months =
        use repo = new Repository (repoPath)

        printfn $"getCommitLogs repoPath={repoPath} branchName={branchName} months={months}"

        match branchName with
        | Some branchName ->
            repo.Branches
            |> Seq.tryFind (fun branch -> branch.RemoteName = branchName)
            |> Option.map (fun branch -> branch.Commits |> Seq.rev)
            |> Option.defaultValue Seq.empty
        | None -> repo.Commits |> Seq.rev
        |> Seq.filter
            (fun commit ->
                match months with
                | None -> true
                | Some months when commit.Committer.When.DateTime > DateTime.Now.AddDays -(float months * 30.) -> true
                | _ -> false)
        |> Seq.map (fun commit -> $"{commit.Committer.When.DateTime} - {commit.Sha}{nl}{nl}{nl}{commit.Message}")
        |> Seq.toList

    let savePdfIo commitLogs (outputPath: string) =
        let document = Document ()
        let section = document.AddSection ()

        let id = ((string (Guid.NewGuid ())).Split "-").[0]

        printfn $"!{id}: lines start"

        for row in commitLogs do
            section.AddParagraph row |> ignore
            section.AddParagraph "</>" |> ignore
            printfn $"!{id}: paragraph: {row}"
            section.AddPageBreak ()
            printfn $"!{id}: page break"

        printfn $"!{id}: lines end"

        let pdfRenderer = PdfDocumentRenderer true
        pdfRenderer.Document <- document
        pdfRenderer.RenderDocument ()
        pdfRenderer.PdfDocument.Save outputPath

    let start repoPath branchName outputPath months =
        async {
            Encoding.RegisterProvider CodePagesEncodingProvider.Instance
            let commitLogs = getCommitLogs repoPath branchName months

            savePdfIo (commitLogs |> Seq.toArray) outputPath
        }

module Args =
    type Arguments =
        | [<Mandatory>] Repo_Path of string
        | [<Mandatory>] Output_Path of string
        | Branch of string
        | Months of int
        interface IArgParserTemplate with
            member this.Usage =
                match this with
                | Repo_Path _ -> "Path of the repository."
                | Branch _ -> "Branch name."
                | Output_Path _ -> "Full path of the PDF file."
                | Months _ -> "Last X months."

module Program =
    [<EntryPoint>]
    let main argv =
        let args = Startup.parseArgsIo argv

        Git2Pdf.start
            (args.GetResult Args.Repo_Path)
            (args.TryGetResult Args.Branch)
            (args.GetResult Args.Output_Path)
            (args.TryGetResult Args.Months)
        |> Async.RunSynchronously

        0
