module Day14
open Xunit
open System
open Utils
open System.Text.RegularExpressions

let filePath = "Inputs\Day14.txt"

type Line =
    | Mask of string
    | Mem of int64 * int64

let parse str =
    let maskMatch = Regex.Match(str, @"mask = (\w+)")
    if maskMatch.Success then
        Mask maskMatch.Groups.[1].Value
    else
        let memMatch = Regex.Match(str, @"mem\[(\d+)] = (\d+)")
        Mem (int64 memMatch.Groups.[1].Value, int64 memMatch.Groups.[2].Value)

/// Перевод числа в двоичную систему
let intToTwoBase (i: int64) =
    let b = Convert.ToString(i, 2)
    String.replicate (36-b.Length) "0" + b

/// Перевод числа из двоичной системы
let twoBaseToInt str =
    Convert.ToInt64(str, 2)

/// Добавление маски к значению
let addMaskValue mask value = 
    Seq.map2 (fun m v -> if m = 'X' then v else m) mask value |> String.Concat

/// Добавление маски к ключу
let addMaskKey mask key = 
    let step c (acc: string list) = 
        if c = 'X' then List.map (fun x -> x + "1") acc @ List.map (fun x -> x + "0") acc
        else List.map (fun x -> x + (string c)) acc
    Seq.fold2 (fun acc m k -> step (if m = '0' then k else m) acc) [""] mask key

/// Вычисление значения
let calcValue mask i = 
    i |> intToTwoBase |> addMaskValue mask |> twoBaseToInt

/// Вычисление списка ключей
let calcKey mask i = 
    i |> intToTwoBase |> addMaskKey mask |> List.map twoBaseToInt

let rec step map mask lines =
    match lines with
    | Mask m :: t -> step map m t
    | Mem (key,value) :: t -> step (Map.change key (fun _ -> Some(calcValue mask value)) map) mask t
    | _ -> map |> Map.toSeq |> Seq.sumBy snd

/// Добавление в словарь значения по нескольким ключам
let addMap map keys value = 
    List.fold (fun m k -> Map.change k (fun _ -> Some(value)) m) map keys

let rec step2 map mask lines =
    match lines with
    | Mask m :: tail -> step2 map m tail
    | Mem (key,value) :: tail -> step2 (addMap map (calcKey mask key) value) mask tail
    | _ -> map |> Map.toSeq |> Seq.sumBy snd

/// Вычисление первой части задачи
let calc input=
    input |> List.map parse |> step Map.empty<int64,int64> ""  

/// Вычисление второй части задачи
let calc2 input =  
    input |> List.map parse |> step2 Map.empty<int64,int64> ""  

/// Запуск решения задачи
let run() =
    let data = readLines filePath |> Seq.toList
    calc data  |> printfn "%d"
    calc2 data |> printfn "%d"

[<Fact>]
let ``Day 14 Test`` () =
    Assert.Equal(intToTwoBase 11L, "000000000000000000000000000000001011") 
    Assert.Equal(intToTwoBase 101L, "000000000000000000000000000001100101") 
    Assert.Equal(twoBaseToInt "000000000000000000000000000000001011", 11L) 
    Assert.Equal(twoBaseToInt "000000000000000000000000000001100101", 101L)
    let data = [
        "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X";
        "mem[8] = 11";
        "mem[7] = 101";
        "mem[8] = 0"]
    Assert.Equal(calc data, 165L)
    let data2 = [
        "mask = 000000000000000000000000000000X1001X";
        "mem[42] = 100";
        "mask = 00000000000000000000000000000000X0XX";
        "mem[26] = 1"]
    Assert.Equal(calc2 data2, 208L)

[<Fact>]
let ``Day 14 Fact`` () =
    let data = readLines filePath |> Seq.toList
    Assert.Equal(calc data, 13105044880745L)
    Assert.Equal(calc2 data, 3505392154485L)
