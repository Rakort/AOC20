module Day15
open Xunit
open System
open Utils
open System.Text.RegularExpressions

let filePath = "Inputs\Day15.txt"

let rec step list =
    let head = List.head list
    let tail = List.tail list
    if list.Length = 2020 then
        head
    else
        let s = List.tryFindIndex (fun x -> x = head) tail
        if s.IsSome then
            step <| s.Value+1 :: list
        else
            step <| 0 :: list

let rec step2 (map: Map<int64, int64>) last i =
    if i = 30000000L-1L then
        last
    else
        if map.ContainsKey(last) then
            step2 (Map.add last i map) (i - Map.find last map) (i+1L)
        else
            step2 (Map.add last i map) 0L (i+1L)

/// Вычисление первой части задачи
let calc (input: string)=
    input.Split(',') |> Array.toList |> List.map int |> List.rev |> step 

/// Вычисление второй части задачи
let calc2 (input: string) =  
    let map = Map.empty<int64,int64> |> Map.add 3L 0L |> Map.add 2L 1L
    step2 map 1L 2L

/// Запуск решения задачи
let run() =
    let data = readLines filePath |> Seq.head
    calc data  |> printfn "%d"
    calc2 data |> printfn "%d"

[<Fact>]
let ``Day 15 Test`` () =
    Assert.Equal(calc "1,3,2", 1) 
    Assert.Equal(calc "2,1,3", 10) 
    Assert.Equal(calc2 "3,2,1", 18L) 

[<Fact>]
let ``Day 15 Fact`` () =
    let data = readLines filePath |> Seq.head
    Assert.Equal(calc data, 639)
    Assert.Equal(calc2 data, 2578L)
