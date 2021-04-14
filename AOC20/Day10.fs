module Day10
open Xunit
open System
open Utils

let filePath = "Inputs\Day10.txt"

/// Преобразует список адаптеров в список разниц напряжения между соседними адаптерами
let rec findAdapter out sourse =
    match sourse with
    | x :: y :: tail -> findAdapter (y-x::out) (y::tail)
    | _ -> out

/// Возвращает адаптеры которые можно подключить к адаптеру adapter
let findAdapters sourse adapter =
    List.where (fun x -> x > adapter && x <= adapter+3) sourse 

/// Преобразует список адаптеров в список вариантов соединения адаптеров 
let mapAdaptersInVariants sourse = 
     sourse |> List.sort |> List.map (fun x -> findAdapters sourse x |> List.length)    

/// Возвращает количество способов расположения адаптеров
let rec countWays sourse i = 
    match sourse with
    | 3 :: 3 :: 2 :: 1 :: 1 :: tail -> countWays tail (i * 7L)
    | 3 :: 2 :: 1 :: 1 :: tail -> countWays tail (i * 4L)
    | 2 :: 1 :: 1 :: tail -> countWays tail (i * 2L)
    | 1 :: tail -> countWays tail i
    | _ -> i

/// Вычисление первой части задачи
let calc input =
    let data = List.append input [List.max input + 3; 0]
    data |> List.sort |> findAdapter [] |> List.countBy id |> List.where(fun (x, _) -> x=1 || x=3) |> List.map snd |> List.fold (*) 1

/// Вычисление второй части задачи
let calc2 input =
    let data = List.append input [List.max input + 3; 0]
    countWays (mapAdaptersInVariants data) 1L

/// Запуск решения задачи
let run() =
    let data = readLines filePath |> Seq.map int |> Seq.toList
    calc data  |> printfn "%d"
    calc2 data |> printfn "%d"

[<Fact>]
let ``Day 10 Test`` () =
    let data = [16; 10; 15; 5; 1; 11; 7; 19; 6; 12; 4]
    let data2 = [28; 33; 18; 42; 31; 14; 46; 20; 48; 47; 24; 23; 49; 45; 19; 38; 39; 11; 1; 32; 25; 35; 8; 17; 7; 9; 4; 2; 34; 10; 3;]
    Assert.Equal(calc data, 35)
    Assert.Equal(calc data2, 220)
    Assert.Equal(calc2 data, 8L)
    Assert.Equal(calc2 data2, 19208L)    

[<Fact>]
let ``Day 10 Fact`` () =
    let data = readLines filePath |> Seq.map int |> Seq.toList
    Assert.Equal(calc data, 2040)
    Assert.Equal(calc2 data, 28346956187648L)
