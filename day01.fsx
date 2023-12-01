open System
open System.IO

// fun with sequences
let firstLastDigit (letters: string) =
    let chars = letters |> Seq.toList
    let fnum = chars |> List.find Char.IsDigit |> Char.GetNumericValue |> int
    let lnum = chars |> List.findBack Char.IsDigit |> Char.GetNumericValue |> int
    fnum, lnum

// now you got two problems
let numberExpression = "([0-9]|one|two|three|four|five|six|seven|eight|nine|ten)"
let wordToNumber word = 
    match word with
    | "one" -> Some(1)
    | "two" -> Some(2)
    | "three" -> Some(3)
    | "four" -> Some(4)
    | "five" -> Some(5)
    | "six" -> Some(6)
    | "seven" -> Some(7)
    | "eight" -> Some(8)
    | "nine" -> Some(9)
    | _ -> None

let firstLastNumbers (letters: string) = 

let combineDigitPair digits =
    let f, l = digits
    (f * 10) + l

let partA (lines: seq<string>) =
    lines |> Seq.map firstLastDigit |> Seq.map combineDigitPair |> Seq.sum

let example = [
    "1abc2";
    "pqr3stu8vwx";
    "a1b2c3d4e5f";
    "treb7uchet"]

142 = partA example

let lines = (IO.File.ReadAllLines "day01inp.txt")
partA lines