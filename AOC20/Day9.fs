module Day9
open Xunit
open System
open Utils

let filePath = "Inputs\Day9.txt"

/// Содержит ли sourse пару чисел которые в сумме дают sum
let isExistSum sourse sum = 
    Seq.allPairs sourse sourse |> Seq.where (fun (f,s) -> f <> s) |> Seq.exists (fun (f,s) -> f+s = sum)

/// Возвращает не действительное число 
let rec findNotValid sourse step =
    if isExistSum (List.take step sourse) (sourse.[step]) then
        findNotValid (List.tail sourse) step
    else
        sourse.[step]

/// Удаляет последний элемент списка
let removeLast sourse = sourse |> List.rev |> List.tail |> List.rev

/// Удаляет последний элемент списка пока сумма элементов больше sum
let rec removeLastUntilMoreSum sum sourse =
    if List.sum sourse > sum then
        removeLast sourse |> removeLastUntilMoreSum sum
    else
        sourse

/// Возвращает непрерывный набор элементов, сумма которых равна sum
let rec findContiguousSet sourse (sum: int64) acc =    
    if List.sum acc = sum then
        acc
    else
        let res = List.head sourse :: acc
        findContiguousSet (List.tail sourse) sum (removeLastUntilMoreSum sum res)
        
/// Возвращает уязвимость шифрования
let findEncryptionWeakness input sum: int64 =
    let set = findContiguousSet input sum []
    List.min set + List.max set

/// Вычисление первой части задачи
let calc input =
   findNotValid input 25

/// Вычисление второй части задачи
let calc2 input =
    let sum = calc input
    findEncryptionWeakness input sum

/// Запуск решения задачи
let run() =
    let data = readLines filePath|> Seq.map int64 |> Seq.toList
    calc data  |> printfn "%d"
    calc2 data |> printfn "%d"

[<Fact>]
let ``Day 9 Test`` () =
    let data = [
        35L;
        20L;
        15L;
        25L;
        47L;
        40L;
        62L;
        55L;
        65L;
        95L;
        102L;
        117L;
        150L;
        182L;
        127L;
        219L;
        299L;
        277L;
        309L;
        576L]
    Assert.Equal(findNotValid data 5, 127L)
    Assert.Equal(findEncryptionWeakness data 127L, 62L)
    

//[<Fact>]
//let ``Day 9 Fact`` () =
//    let data = readLines filePath
//    Assert.Equal(calc data, 1584)
//    Assert.Equal(calc2 data, 920)
