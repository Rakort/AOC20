module Day5
open Xunit
open System
open Utils

let filePath = "Inputs\Day5.txt"

// Бинарный поиск значения
let rec binaryFind min max list =
    if min = max then min
    else
        let mid = (max-min)/2 + (max-min)%2
        match Seq.head list with
        | 'B' | 'R' -> binaryFind (min + mid ) max (Seq.tail list)
        | 'F' | 'L' -> binaryFind min (max-mid) (Seq.tail list)
        | _ -> invalidArg "list" "Неизвестный тип комманды" 

// Поиск строки
let calcRow path =
    binaryFind 0 127 (path |> Seq.take 7)

// Поиск столбца
let calcColumn path = 
    binaryFind 0 7 (path |> Seq.skip 7)

// Расчет идентификатора места
let calcSeatId path =
    calcRow path * 8 + calcColumn path

// Поиск пропушенного места
let rec findMySeat sourse =
    match sourse with
    | prev :: next :: tail -> if prev + 2 = next 
                              then prev+1 
                              else findMySeat <| List.tail sourse  
    | _ -> failwith "Место не найдено"

let calc input  =
    input |> Seq.map calcSeatId |> Seq.max

let calc2 input =
    input |> Seq.map calcSeatId |> Seq.sort |> Seq.toList |> findMySeat    

let run() =
    let data = readLines filePath
    calc data |> printfn "%d"
    calc2 data |> printfn "%d"    

[<Fact>]
let ``Day 5 Test`` () =
    let data = "FBFBBFFRLR"
    Assert.Equal(calcRow data, 44)
    Assert.Equal(calcColumn data, 5)
    Assert.Equal(calcSeatId data, 357)
    let data2 = "BFFFBBFRRR"
    Assert.Equal(calcRow data2, 70)
    Assert.Equal(calcColumn data2, 7)
    Assert.Equal(calcSeatId data2, 567)
    let data3 = "FFFBBBFRRR"
    Assert.Equal(calcRow data3, 14)
    Assert.Equal(calcColumn data3, 7)
    Assert.Equal(calcSeatId data3, 119)
    let data4 = "BBFFBBFRLL"
    Assert.Equal(calcRow data4, 102)
    Assert.Equal(calcColumn data4, 4)
    Assert.Equal(calcSeatId data4, 820)
    

[<Fact>]
let ``Day 5 Fact`` () =
    let data = readLines filePath
    Assert.Equal(calc data, 926)
    Assert.Equal(calc2 data, 657)
