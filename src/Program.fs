﻿namespace Git2Pdf.Cli

open Argu
open System
open System.Text
open Suigetsu.Core
open MigraDoc.DocumentObjectModel
open MigraDoc.Rendering
open Serilog
open Suigetsu.CoreCLR

module Git2Pdf =
    let runCmdIoAsync repoPath cmd = async {
        let cmd =
            [ "cd " + repoPath ] @ cmd
        let! errorCode, result = Runtime.executePowerShellAsync cmd
        
        return
            match errorCode with
            | 0 -> Some result
            | _ -> None
    }
    
    let getCommitLogsIoAsync repoPath (commitList: string) = async {
        return!
            commitList.Split Environment.NewLine
            |> Array.rev
            |> Array.map (fun commit ->
                              runCmdIoAsync repoPath [ "git log -1 "
                                                       + commit
                                                       + """ --pretty="%ci%n%H - %h%n%n%n%B" """ ])
            |> Async.throttle 4
            |> Async.Sequential
            |> Async.choose Array.choose
    }
    
    let savePdfIo commitLogs (outputPath: string) =
        let document = Document ()
        let section = document.AddSection ()
        
        for log in commitLogs do
            section.AddParagraph log |> ignore
            section.AddPageBreak ()
                
        let pdfRenderer = PdfDocumentRenderer true
        pdfRenderer.Document <- document
        pdfRenderer.RenderDocument ()
        pdfRenderer.PdfDocument.Save outputPath
    
    let startIoAsync (branch: string) (repoPath: string) (outputPath: string) = async {
        Encoding.RegisterProvider CodePagesEncodingProvider.Instance
        
        let! commitList = runCmdIoAsync repoPath [ "git rev-list " + branch ]
        
        match commitList with
        | None ->
            Log.Error ("Error retrieving commit list")
            
        | Some commitList ->
            let! commitLogs = getCommitLogsIoAsync repoPath commitList
            savePdfIo commitLogs outputPath
    }

module Args =
    type Arguments =
        | [<Mandatory>] Repo_Path of string
        | [<Mandatory>] Branch of string
        | [<Mandatory>] Output_Path of string
    with
        interface IArgParserTemplate with
            member this.Usage =
                match this with
                | Repo_Path _ -> "Path of the repository."
                | Branch _ -> "Branch name."
                | Output_Path _ -> "Full path of the PDF file."
        
module Program = 
    [<EntryPoint>]
    let main argv =
        fun () ->
            let args = Startup.parseArgsIo argv
            
            Git2Pdf.startIoAsync
                (args.GetResult Args.Branch)
                (args.GetResult Args.Repo_Path)
                (args.GetResult Args.Output_Path)
            |> Async.RunSynchronously
        |> Startup.withLogging false
