#r "paket:
nuget Fake ~> 5 //
nuget Fake.Core ~> 5 //
nuget Fake.Core.Target  //
nuget Fake.DotNet //
nuget Fake.DotNet.AssemblyInfoFile //
nuget Fake.DotNet.Cli //
nuget Fake.DotNet.NuGet //
nuget Fake.IO.FileSystem //
nuget Fake.Tools.Git ~> 5 //"
#load "./.fake/build.fsx/intellisense.fsx"


#if !FAKE
#r "netstandard"
#r "Facades/netstandard" // https://github.com/ionide/ionide-vscode-fsharp/issues/839#issuecomment-396296095
#endif

open Fake.Core
open Fake.DotNet
open Fake.IO

[<RequireQualifiedAccess>]
type Targets = 
   Build 
   | Package
   | Push
   | Test
   | Generic of string

let targetName = 
    function
        Targets.Build -> "build"
        | Targets.Package -> "package"
        | Targets.Push -> "push"
        | Targets.Test -> "test"
        | Targets.Generic s -> s

open Fake.Core.TargetOperators
let inline (==>) (lhs : Targets) (rhs : Targets) =
    Targets.Generic((targetName lhs) ==> (targetName rhs))

let inline (?=>) (lhs : Targets) (rhs : Targets) =
    Targets.Generic((targetName lhs) ?=> (targetName rhs))

let create target = 
    target
    |> targetName
    |> Target.create

let runOrDefaultWithArguments =
    targetName
    >> Target.runOrDefaultWithArguments 

let run command workingDir args = 
    let arguments = 
        match args |> String.split ' ' with
        [""] -> Arguments.Empty
        | args -> args |> Arguments.OfArgs
    RawCommand (command, arguments)
    |> CreateProcess.fromCommand
    |> CreateProcess.withWorkingDirectory workingDir
    |> CreateProcess.ensureExitCode
    |> Proc.run
    |> ignore

let nugetFeedUrl = "https://kmddk.pkgs.visualstudio.com/45c29cd0-03bf-4f63-ac71-3c366095dda9/_packaging/KMD_Package_Feed/nuget/v2"

let buildConfiguration = 
        DotNet.BuildConfiguration.Release
  
open System.IO
let verbosity = Quiet
    
let package conf outputDir projectFile =
    DotNet.publish (fun opts -> 
                        { opts with 
                               OutputPath = Some outputDir
                               Configuration = conf
                               MSBuildParams = 
                                   { opts.MSBuildParams with
                                          Verbosity = Some verbosity
                                   }    
                        }
                   ) projectFile
let srcPath = "src/"

create Targets.Build (fun _ ->    
    let projectFile = srcPath + "hobbes.core.fsproj"
    package buildConfiguration "./package" projectFile
)
let paket workDir args = 
    run "dotnet" workDir ("paket " + args) 
    
create Targets.Package (fun _ ->
    let packages = Directory.EnumerateFiles(srcPath, "*.nupkg")
    let dateTime = System.DateTime.UtcNow
    let version = sprintf "1.0.%i.%i.%i-default" dateTime.Year dateTime.DayOfYear ((int) dateTime.TimeOfDay.TotalSeconds)
    let packageVersion = Environment.environVarOrDefault "APPVEYOR_BUILD_VERSION" version
    File.deleteAll packages
    sprintf "pack --version %s ." packageVersion
    |> paket srcPath 
)

create Targets.Push (fun _ ->
    let nupkgFilePath = 
        Directory.EnumerateFiles(srcPath, "*.nupkg")
        |> Seq.exactlyOne
    sprintf "push --api-key $KEY %s" nupkgFilePath
    |> paket "./"
)

create Targets.Test (fun _ ->
    DotNet.test id "tests/hobbes.core.tests.fsproj"
)

Targets.Build
    ==> Targets.Package

Targets.Build
    ?=> Targets.Test

Targets.Package
    ?=>Targets.Push

Targets.Package
|> runOrDefaultWithArguments 