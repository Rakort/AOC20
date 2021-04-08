module Day7
open Xunit
open System
open Utils
open System.Text.RegularExpressions

let filePath = "Inputs\Day7.txt"

// Разбор строки правил
let parse (line: string) = 
    let spl = line.Split(" contain ")
    let owner = spl.[0].Replace(" bags", "")
    let childs = 
        if spl.[1].Contains("no other bags") then Array.empty<string * int>
        else spl.[1].Split(',') 
             |> Array.map (fun x -> 
                           let m = Regex.Match(x, "(\d) (\w+ \w+) bags?")
                           m.Groups.[2].Value, (int)m.Groups.[1].Value)
    (owner, childs)

// Количество цветов сумок содержащих сумку bag 
let rec countColor map bag =
    let colors = map |> Map.filter (fun k v -> Array.contains bag v) |> Map.toSeq |> Seq.map fst
    Seq.concat [colors; (colors |> Seq.map (countColor map) |> Seq.concat)] |> Seq.distinct

// Количество требуемых сумок внутри bag
let rec countBag map bag =
    map |> Map.find bag |> Seq.map (fun (b, c) -> c * (countBag map b) + c) |> Seq.sumBy id   

let calc input  =
    let rules = input |> Seq.map parse |> Seq.map (fun (f,s) -> f, Array.map fst s) |> Map.ofSeq  
    countColor rules "shiny gold" |> Seq.length

let calc2 input  =
    let rules = input |> Seq.map parse |> Map.ofSeq  
    countBag rules "shiny gold"     

let run() =
    let data = readLines filePath
    calc data |> printfn "%d"
    calc2 data |> printfn "%d"    

[<Fact>]
let ``Day 7 Test`` () =
    let data = seq{
        "light red bags contain 1 bright white bag, 2 muted yellow bags.";
        "dark orange bags contain 3 bright white bags, 4 muted yellow bags.";
        "bright white bags contain 1 shiny gold bag.";
        "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.";
        "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.";
        "dark olive bags contain 3 faded blue bags, 4 dotted black bags.";
        "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.";
        "faded blue bags contain no other bags.";
        "dotted black bags contain no other bags."}
    Assert.Equal(calc data, 4)
    Assert.Equal(calc2 data, 32)
    

[<Fact>]
let ``Day 7 Fact`` () =
    let data = readLines filePath
    Assert.Equal(calc data, 155)
    Assert.Equal(calc2 data, 54803)
