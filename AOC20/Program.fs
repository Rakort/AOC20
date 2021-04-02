// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp
open System

// Define a function to construct a message to print
let from whom =
    sprintf "from %s" whom

[<EntryPoint>]
let main argv =
    let message = from "F#" // Call the function
    printfn "Hello world %s" message

    let timer = System.Diagnostics.Stopwatch.StartNew()

    Day3.run()

    timer.Stop()
    printfn "elapsed=%O" <| timer.Elapsed
    0 // return an integer exit code