module Day8
open Xunit
open System
open Utils
open System.Text.RegularExpressions

let filePath = "Inputs\Day8.txt"

/// Разбор строки инструкции
let parse (line: string) = 
    let spl = line.Split(" ")
    (spl.[0], (int)spl.[1])

/// Выполнение одного шага инструкции
let step (act, arg) i acc =
    match act with
    | "acc" ->(i+1, acc+arg) 
    | "jmp" -> (i+arg, acc) 
    | _ -> (i+1, acc)
    
/// Выполнение инструкций пока не обнаружится цикл или не закончатся инструкции
let calcInstruction (getItem: int -> (string*int) option) =
    // Список для посещенных позиций
    let mutable pos = List.empty<int>
    let rec stepInstruction (i, acc) =
        // Если позиция уже встречалась
        if List.contains i pos then 
            i, acc
        else
            pos <- i :: pos
            match getItem i with
            | Some(x) -> step x i acc |> stepInstruction            
            | None -> i, acc // инструкции закончились
    stepInstruction (0, 0)


/// Возвращает значение из массива;
/// Если changeInd = i изменяет операцию;
/// Если i выходит за границы массива возвращает None;
let getItem (sourse: (string*int)[]) changeInd i =    
    if i >= sourse.Length then 
        None
    else
        let (act, arg) = sourse.[i]
        match (act, changeInd = i) with
        | "nop", true -> Some("jmp", arg)
        | "jmp", true -> Some("nop", arg)
        | _, _ -> Some(act, arg)

/// Вычисление первой части задачи
let calc input =
    let instruction = input |> Seq.map parse |> Seq.toArray    
    let (_, acc) = calcInstruction (getItem instruction -1)
    acc

/// Вычисление второй части задачи
let calc2 input =
    let instruction = input |> Seq.map parse |> Seq.toArray
    // Ищем когда инструкция выполниться полностью
    let rec findOutOfInstruction i =
        let (ind, acc) = calcInstruction (getItem instruction i)
        if ind >= instruction.Length then acc
        else findOutOfInstruction (i+1)
    findOutOfInstruction 0

/// Запуск решения задачи
let run() =
    let data = readLines filePath
    calc data |> printfn "%d"
    calc2 data |> printfn "%d"

[<Fact>]
let ``Day 8 Test`` () =
    let data = seq{
        "nop +0";
        "acc +1";
        "jmp +4";
        "acc +3";
        "jmp -3";
        "acc -99";
        "acc +1";
        "jmp -4";
        "acc +6"}
    Assert.Equal(calc data, 5)
    Assert.Equal(calc2 data, 8)
    

[<Fact>]
let ``Day 8 Fact`` () =
    let data = readLines filePath
    Assert.Equal(calc data, 1584)
    Assert.Equal(calc2 data, 920)
