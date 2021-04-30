module Day13
open Xunit
open System
open Utils

let filePath = "Inputs\Day13.txt"

/// Вычисление первой части задачи
let calc (input: string list)=
    let startTime = int input.[0]
    let buses = input.[1].Split(',') |> Array.where (fun x -> x <> "x") |> Array.map int 
    let cross = buses |> Array.map (fun x -> x, (startTime/x+1)*x) |> Array.minBy (snd)
    fst cross * (snd cross - startTime)
    
/// Поиск пересечения двух автобусов
let rec findCrossTwo startValue step bus offset  =
    let next = startValue + step
    if (next+offset)%bus=0L then next
    else findCrossTwo next step bus offset  

/// Возвращает первое пересечение и время до последующих
let findCrosses (startValue, step) (sndBus, offset)  = 
    let fstCross = findCrossTwo startValue step sndBus offset
    let sndCross = findCrossTwo fstCross step sndBus offset
    fstCross, sndCross-fstCross

/// Поиск пересечения маршрутов автобусов
let findCross (input: string) = 
    let buses = input.Split(',') |> Array.mapi (fun i x -> (x, i)) |> Array.where (fun (x,_) -> x <> "x") |> Array.map (fun (f,i) -> (int64 f, int64 i))
    Array.tail buses |> Array.fold findCrosses (0L, fst buses.[0]) |> fst

/// Вычисление второй части задачи
let calc2 (input: string list) =  
    findCross input.[1]

/// Запуск решения задачи
let run() =
    let data = readLines filePath |> Seq.toList
    calc data  |> printfn "%d"
    calc2 data |> printfn "%d"

[<Fact>]
let ``Day 13 Test`` () =
    let data = ["939"; "7,13,x,x,59,x,31,19";]    
    Assert.Equal(calc data, 295)   
    Assert.Equal(findCross "13,19", 208L) 
    Assert.Equal(findCross "17,x,13", 102L) 
    Assert.Equal(findCross "17,x,x,19", 187L) 
    Assert.Equal(findCross "17,x,13,19", 3417L) 
    Assert.Equal(findCross "67,7,59,61", 754018L) 
    Assert.Equal(findCross "67,x,7,59,61", 779210L) 
    Assert.Equal(findCross "67,7,x,59,61", 1261476L) 
    Assert.Equal(findCross "1789,37,47,1889", 1202161486L) 

[<Fact>]
let ``Day 13 Fact`` () =
    let data = readLines filePath |> Seq.toList
    Assert.Equal(calc data, 333)
    Assert.Equal(calc2 data, 690123192779524L)
