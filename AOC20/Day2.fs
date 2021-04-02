module Day2
open Xunit
open System
open Utils
open System.Text.RegularExpressions

let filePath = "Inputs\Day2.txt"

type pas = 
    val min:int
    val max:int
    val char:char
    val password:string
    new (min, max, char, password) = { min = min; max = max; char = char; password = password }
    new (str) = 
        let m = Regex.Match(str, @"(\d+)-(\d+) (\w): (\w+)")
        { min = int m.Groups.[1].Value; max = int m.Groups.[2].Value; char = char m.Groups.[3].Value; password = m.Groups.[4].Value }
    member this.isValid () = 
       // Считаем сколько раз нужный символ содержится в пароле 
       let countChar = this.password |> Seq.where (fun x -> x = this.char) |> Seq.length       
       // Проверяем валидный ли пароль
       countChar >= this.min && countChar <= this.max
    member this.isValid2 () = 
        // Проверяем валидный ли пароль
        let validMin = this.password.[this.min-1] = this.char
        let validMax = this.password.[this.max-1] = this.char
        validMin <> validMax

let calc input =
    input |> Seq.map (fun x -> new pas(x)) // Преобразуем в класс
          |> Seq.where(fun x -> x.isValid()) // Отбираем валидные
          |> Seq.length // Считаем количество

let calc2 input =
    input |> Seq.map (fun x -> new pas(x)) // Преобразуем в класс
          |> Seq.where(fun x -> x.isValid2()) // Отбираем валидные
          |> Seq.length // Считаем количество

let run() =
    let data = readLines filePath 
    calc data |> printfn "%d"
    calc2 data |> printfn "%d"    

[<Fact>]
let ``Day 2 Test`` () =
    let data = seq{"1-3 a: abcde"; "1-3 b: cdefg"; "2-9 c: ccccccccc"}
    Assert.Equal(calc data, 2)
    Assert.Equal(calc2 data, 1)

[<Fact>]
let ``Day 1 Fact`` () =
    let data = readLines filePath 
    Assert.Equal(calc data, 572)
    Assert.Equal(calc2 data, 306)