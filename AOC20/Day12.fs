module Day12
open Xunit
open System
open Utils

let filePath = "Inputs\Day12.txt"

type vector2 = {x:int; y:int}

/// Сложение векторов
let sumVector v1 v2 = {x = v1.x+v2.x; y = v1.y+v2.y}

/// Разбор строки с командой
let parse (str:string) =
    (str.[0], Seq.skip 1 str |> String.Concat |> int)

/// Поворот вектора на угол в градусах
let rotateVector (vector:vector2) (angle:int) = 
    [|1..(angle+360)/90|] |> Array.fold (fun d _ -> {x = d.y; y=(-d.x)}) vector

/// Выполнение шага инструкции части 1
let step ((dir:vector2),(pos:vector2)) (key, value) =
    match key with
    | 'N' -> (dir, sumVector pos {x=0; y=value})
    | 'S' -> (dir, sumVector pos {x=0; y=(-value)})
    | 'E' -> (dir, sumVector pos {x=value; y=0})
    | 'W' -> (dir, sumVector pos {x=(-value); y= 0})
    | 'L' -> (rotateVector dir -value, pos)
    | 'R' -> (rotateVector dir value, pos)
    | 'F' -> (dir, sumVector pos {x=dir.x*value; y=dir.y*value})
    | _   -> invalidArg "key" "Неизвестный тип комманды" 

/// Выполнение шага инструкции части 2
let step2 ((waypoint:vector2),(pos:vector2)) (key, value) =
    match key with
    | 'N' -> (sumVector waypoint {x=0; y=value}, pos)
    | 'S' -> (sumVector waypoint {x=0; y=(-value)}, pos)
    | 'E' -> (sumVector waypoint {x=value; y=0}, pos)
    | 'W' -> (sumVector waypoint {x=(-value); y=0}, pos)
    | 'L' -> (rotateVector waypoint -value, pos)
    | 'R' -> (rotateVector waypoint value, pos)
    | 'F' -> (waypoint, sumVector pos {x=waypoint.x * value; y=waypoint.y * value})
    | _   -> invalidArg "key" "Неизвестный тип комманды" 

/// Вычисление первой части задачи
let calc input :int=
    let pos = input |> Seq.map parse |> Seq.fold step ({x=1;y=0}, {x=0;y=0}) |> snd
    Math.Abs(pos.x) + Math.Abs(pos.y)    

/// Вычисление второй части задачи
let calc2 input =  
    let pos = input |> Seq.map parse |> Seq.fold step2 ({x=10;y=1}, {x=0;y=0}) |> snd
    Math.Abs(pos.x) + Math.Abs(pos.y)

/// Запуск решения задачи
let run() =
    let data = readLines filePath
    calc data  |> printfn "%d"
    calc2 data |> printfn "%d"

[<Fact>]
let ``Day 12 Test`` () =
    let data = ["F10"; "N3"; "F7"; "R90"; "F11"]    
    Assert.Equal(calc data, 25)
    Assert.Equal(calc2 data, 286) 

[<Fact>]
let ``Day 12 Fact`` () =
    let data = readLines filePath 
    Assert.Equal(calc data, 1687)
    Assert.Equal(calc2 data, 20873)
