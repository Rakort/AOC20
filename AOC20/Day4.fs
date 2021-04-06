module Day4
open Xunit
open System
open System.Text.RegularExpressions
open Utils

let filePath = "Inputs\Day4.txt"

let needFilds = [|"byr";"iyr";"eyr";"hgt";"hcl";"ecl";"pid"|]

// Проверка наличия всех обязательных полей
let checkRequiredFilds (passport: (string * string)[]) = 
    // Массив, содержащий элементы, отсутствующие в needFilds.
    let valid = Array.except (Array.map fst passport) needFilds
    valid.Length = 0

// Проверка действительности поля
let checkField (key, value) =
    match key with
        | "byr" ->  if Regex.IsMatch(value, "\d{4}") then
                        (int)value >= 1920 && int(value) <= 2002
                    else false
        | "iyr" ->  if Regex.IsMatch(value, "\d{4}") then
                        (int)value >= 2010 && int(value) <= 2020
                    else false
        | "eyr" ->  if Regex.IsMatch(value, "\d{4}") then
                        (int)value >= 2020 && int(value) <= 2030
                    else false
        | "hgt" ->  let m = Regex.Match(value, "(\d+)(cm|in)")
                    if m.Success then
                        let hgt = (int)m.Groups.[1].Value
                        if m.Groups.[2].Value = "cm" then                         
                            hgt >= 150 && hgt <= 193
                        else
                            hgt >= 59 && hgt <= 76
                    else false
        | "hcl" ->  Regex.IsMatch(value, "#[0-9a-f]{6}")
        | "ecl" ->  List.contains value ["amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth"]
        | "pid" ->  Regex.IsMatch(value, "^\d{9}$")
        | "cid" ->  true
        | _ -> true

// Проверка действительности всех полей
let checkValidField passport = Array.forall checkField passport    

// Разбор паспорта из строки
let parsePasport str =
    let items = Regex.Split(str, @"\s") |> Array.where (fun x -> x <> "")
    items |> Array.map (fun x -> 
        let m = x.Split(":")
        (m.[0], m.[1]))

// Разбор входной строки
let parseInput (input: string) =
    input.Split("\r\n\r\n") |> Array.map parsePasport

let calc (input: string) =
    parseInput input |> Array.where checkRequiredFilds |> Array.length

let calc2 (input: string) =
    parseInput input |> Array.where checkRequiredFilds |> Array.where checkValidField |> Array.length

let run() =
    let data = readAll filePath
    calc data |> printfn "%d"
    calc2 data |> printfn "%d"    

[<Fact>]
let ``Day 4 Test`` () =
    let data = 
        "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\r\n" + 
        "byr:1937 iyr:2017 cid:147 hgt:183cm\r\n\r\n" + 

        "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\r\n" + 
        "hcl:#cfa07d byr:1929\r\n\r\n" + 

        "hcl:#ae17e1 iyr:2013\r\n" +  
        "eyr:2024\r\n" + 
        "ecl:brn pid:760753108 byr:1931\r\n" + 
        "hgt:179cm\r\n\r\n" +

        "hcl:#cfa07d eyr:2025 pid:166559648\r\n"  + 
        "iyr:2011 ecl:brn hgt:59in"

    Assert.Equal(calc data, 2)

[<Fact>]
let ``Day 4 CheckField``() =
    Assert.True(checkField("byr", "2002"))
    Assert.False(checkField("byr", "2003"))

    Assert.True(checkField("hgt", "60in"))
    Assert.True(checkField("hgt", "190cm"))
    Assert.False(checkField("hgt", "190in"))
    Assert.False(checkField("hgt", "190"))

    Assert.True(checkField("hcl", "#123abc"))
    Assert.False(checkField("hcl", "#123abz"))
    Assert.False(checkField("hcl", "123abc"))

    Assert.True(checkField("ecl", "brn"))
    Assert.False(checkField("ecl", "wat"))

    Assert.True(checkField("pid", "000000001"))
    Assert.False(checkField("pid", "0123456789"))

[<Fact>]
let ``Day 4 CheckUnvalidPassport``() =
    let passport1 = 
        "eyr:1972 cid:100\r\n" + 
        "hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926"
    let passport2 =    
        "iyr:2019\r\n" + 
        "hcl:#602927 eyr:1967 hgt:170cm\r\n" + 
        "ecl:grn pid:012533040 byr:1946"
    let passport3 =    
        "hcl:dab227 iyr:2012\r\n" + 
        "ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277"
    let passport4 =     
        "hgt:59cm ecl:zzz\r\n" + 
        "eyr:2038 hcl:74454a iyr:2023\r\n" + 
        "pid:3556412378 byr:2007"

    Assert.False(parsePasport passport1 |> checkValidField)
    Assert.False(parsePasport passport2 |> checkValidField)
    Assert.False(parsePasport passport3 |> checkValidField)
    Assert.False(parsePasport passport4 |> checkValidField)

[<Fact>]
let ``Day 4 CheckValidPassport``() =
    let passport1 = 
        "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980\r\n" + 
        "hcl:#623a2f"
    let passport2 =        
        "eyr:2029 ecl:blu cid:129 byr:1989\r\n" + 
        "iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm"
    let passport3 =        
        "hcl:#888785\r\n" + 
        "hgt:164cm byr:2001 iyr:2015 cid:88\r\n" + 
        "pid:545766238 ecl:hzl\r\n" + 
        "eyr:2022"
    let passport4 =         
        "iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719" 

    Assert.True(parsePasport passport1 |> checkValidField)
    Assert.True(parsePasport passport2 |> checkValidField)
    Assert.True(parsePasport passport3 |> checkValidField)
    Assert.True(parsePasport passport4 |> checkValidField)

[<Fact>]
let ``Day 4 Fact`` () =
    let data = readAll filePath
    Assert.Equal(calc data, 247)
    Assert.Equal(calc2 data, 145)
