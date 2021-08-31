module Day11
open Xunit
open System
open Utils

let filePath = "Inputs\Day11.txt"

/// Возвращает элемент
let getItem (map: string list) x y = 
    if x < 0 || y < 0 || x >= map.Head.Length || y >= map.Length then '.'
    else map.[y].[x]

/// Возвращает соседний элемент
let getNeighbor (map:string list) x y offssetX offsetY = 
    getItem map (x+offssetX) (y+offsetY)

/// Возвращает соседний элемент в направлении
let getNeighborDir (map:string list) x y offssetX offsetY = 
    let rec getItemDir x y =
        match getItem map x y with
        | '.' when x >= 0 && y >= 0 && x < map.Head.Length && y < map.Length -> getItemDir (offssetX+x) (offsetY+y)
        | char -> char
    getItemDir (offssetX+x) (offsetY+y)

/// Смещения дял поиска соседей
let neighborOffsets = List.allPairs [0;1;-1] [0;1;-1] |> List.where (fun (x,y) -> not (x=0 && y=0))

/// Возвращает количество занятых соседних мест
let countNeg (map:string list) funGetNeighbor x y = 
    let negs = neighborOffsets 
                |> List.map (fun (ox,oy) -> funGetNeighbor map x y ox oy) 
                |> List.countBy id |> List.tryFind (fun (ch, i) -> ch = '#')
    match negs with
    | Some (_, i) -> i
    | None -> 0

/// Возвращает состояние места на следующем шаге
let step (map:string list) maxNeighbor funGetNeighbor y x = 
    let seat = getItem map x y
    if seat = '.' then '.'
    else 
        let countNeg = countNeg map funGetNeighbor x y 
        match seat with
        | 'L' when countNeg = 0 -> '#'
        | '#' when countNeg >= maxNeighbor -> 'L'
        | x -> x

/// Возвращает состояние после одного раунда
let nextStep maxNeighbor funGetNeighbor (map:string list) =
    [for y = 0 to map.Length-1 do 
         yield [0..map.Head.Length-1] |> List.map (step map maxNeighbor funGetNeighbor y) |> String.Concat]  

/// Выполняет правила пока люди не перестают двигаться
let rec occupiedSeats (map: string list) funNextStep =
    let newMap = funNextStep map
    if newMap = map then 
        newMap |> String.Concat |> Seq.countBy id |> Seq.find (fun (ch,_) -> ch = '#') |> snd
    else 
        occupiedSeats newMap funNextStep

/// Вычисление первой части задачи
let calc input =
    occupiedSeats input (nextStep 4 getNeighbor)

/// Вычисление второй части задачи
let calc2 input = 
    occupiedSeats input (nextStep 5 getNeighborDir)

/// Запуск решения задачи
let run() =
    let data = readLines filePath |> Seq.toList
    calc data  |> printfn "%d"
    calc2 data |> printfn "%d"

[<Fact>]
let ``Day 11 Test`` () =
    let data = [
        "L.LL.LL.LL";
        "LLLLLLL.LL";
        "L.L.L..L..";
        "LLLL.LL.LL";
        "L.LL.LL.LL";
        "L.LLLLL.LL";
        "..L.L.....";
        "LLLLLLLLLL";
        "L.LLLLLL.L";
        "L.LLLLL.LL"]    
    Assert.Equal(calc data, 37)
    Assert.Equal(calc2 data, 26) 
    0

[<Fact>]
let ``Day 11 Fact`` () =
    let data = readLines filePath |> Seq.toList
    Assert.Equal(calc data, 2238)
    Assert.Equal(calc2 data, 2013)
