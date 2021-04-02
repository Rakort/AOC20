module Day1
open System
open Utils
open Xunit

let filePath = "Inputs\Day1.txt"

let calc input =
    let mutable res = 0
    for i = 0 to Array.length input - 1 do
        for j = Array.length input - 1 downto i do
            if input.[i] + input.[j] = 2020 then
                res <- input.[i] * input.[j]
    res

let calc2 input =
    let mutable res = 0
    for i = 0 to Array.length input - 1 do
        for j = Array.length input - 1 downto 0 do
            for k = Array.length input - 1 downto 0 do
                if input.[i] + input.[j] + input.[k] = 2020 then
                    res <- input.[i] * input.[j] * input.[k]
    res
   

let run() =
    let data = readLines filePath |> Seq.map int |> Seq.toArray
    calc data |> printfn "%d"
    calc2 data |> printfn "%d"
    

[<Fact>]
let ``Day 1 Test`` () =
    let data = [|1721; 979; 366; 299; 675; 1456|]
    Assert.Equal(calc data, 514579)
    Assert.Equal(calc2 data, 241861950)

[<Fact>]
let ``Day 1 Fact`` () =
    let data = readLines filePath |> Seq.map int |> Seq.toArray
    Assert.Equal(calc data, 1016131)
    Assert.Equal(calc2 data, 276432018)
