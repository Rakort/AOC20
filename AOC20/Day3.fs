module Day3
open Xunit
open System
open Utils

let filePath = "Inputs\Day3.txt"

let calc (input : string[]) offset =
    let (offsetX, offsetY) = offset
    let len = input.[0].Length
    let rec countTree posX posY (count: int) =        
        let x = (posX + offsetX) % len
        let y = posY + offsetY
        if (y >= input.Length) then
            count
        else
            countTree x y (count + if input.[y].[x] = '#' then 1 else 0)
    countTree 0 0 0

let calc2 input =
    [calc input (1,1); calc input (3,1); calc input (5,1); calc input (7,1); calc input (1,2) ]
       |> List.map int64 |> List.fold (*) 1L

let run() =
    let data = readLines filePath |> Seq.toArray
    calc data (3,1) |> printfn "%d"
    calc2 data |> printfn "%d"    

[<Fact>]
let ``Day 3 Test`` () =
    let data = [|
        "..##.......";
        "#...#...#..";
        ".#....#..#.";
        "..#.#...#.#";
        ".#...##..#.";
        "..#.##.....";
        ".#.#.#....#";
        ".#........#";
        "#.##...#...";
        "#...##....#";
        ".#..#...#.#"|]
    Assert.Equal(calc data (3,1), 7)
    Assert.Equal(calc data (1,1), 2)
    Assert.Equal(calc data (5,1), 3)
    Assert.Equal(calc data (7,1), 4)
    Assert.Equal(calc data (1,2), 2)
    Assert.Equal(calc2 data, 336L)

[<Fact>]
let ``Day 3 Fact`` () =
    let data = readLines filePath |> Seq.toArray
    Assert.Equal(calc data (3,1), 299)
    Assert.Equal(calc2 data, 3621285278L)


