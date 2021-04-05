open System

[<EntryPoint>]
let main argv =

    let timer = System.Diagnostics.Stopwatch.StartNew()

    Day4.run()

    timer.Stop()
    printfn "elapsed=%O" <| timer.Elapsed
    0 // return an integer exit code