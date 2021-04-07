module Day6
open Xunit
open System
open Utils
open System.Text.RegularExpressions

let filePath = "Inputs\Day6.txt"

// Подсчет количества повторяющихся символов
let countRepeatChar (group : string) =
    let items = group.Split("\r\n")
    Regex.Replace(group, "\s", "") |> Seq.toArray |> Array.countBy id |> Array.where (fun (_, count) -> count = items.Length ) |> Array.length

// Подсчет количества уникальных символов
let countUniqueChar (group : string) =
    Regex.Replace(group, "\s", "") |> Seq.distinct |> Seq.length

let calc (input : string)  =
    let groups = input.Split("\r\n\r\n")
    groups |> Array.map countUniqueChar |> Array.sum    

let calc2 (input : string)  =
    let groups = input.Split("\r\n\r\n")
    groups |> Array.map countRepeatChar |> Array.sum

let run() =
    let data = readAll filePath
    calc data |> printfn "%d"
    calc2 data |> printfn "%d"    

[<Fact>]
let ``Day 6 Test`` () =
    let data = 
        "abc" + "\r\n\r\n" +

        "a" + "\r\n" + 
        "b" + "\r\n" + 
        "c" + "\r\n\r\n" +

        "ab" + "\r\n" + 
        "ac" + "\r\n\r\n" +

        "a" + "\r\n" + 
        "a" + "\r\n" + 
        "a" + "\r\n" + 
        "a" + "\r\n\r\n" +

        "b"
    Assert.Equal(calc data, 11)
    Assert.Equal(calc2 data, 6)
    

[<Fact>]
let ``Day 6 Fact`` () =
    let data = readAll filePath
    Assert.Equal(calc data, 6161)
    Assert.Equal(calc2 data, 2971)

