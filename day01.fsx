open System
open System.Text.RegularExpressions

/// Part A plays with sequences, and Part B explores regular expressions

/// --- PART A ---

// fun with sequences
let firstLastDigit (letters: string) =
    let chars = letters |> Seq.toList
    let fnum = chars |> List.find Char.IsDigit |> Char.GetNumericValue |> int
    let lnum = chars |> List.findBack Char.IsDigit |> Char.GetNumericValue |> int
    fnum, lnum

let combineDigitPair digits =
    let f, l = digits
    (f * 10) + l

let calibrate (lines: seq<string>) f = 
    lines |> Seq.map f |> Seq.map combineDigitPair |> Seq.sum

let partA (lines: seq<string>) =
    calibrate lines firstLastDigit

let partAExample = [
    "1abc2";
    "pqr3stu8vwx";
    "a1b2c3d4e5f";
    "treb7uchet"]

142 = partA partAExample

let lines = (IO.File.ReadAllLines "day01inp.txt")
partA lines

/// --- PART B ---

// now you got two problems
let numberExpression = "([0-9]|one|two|three|four|five|six|seven|eight|nine)"
let startingNumberRegex = Regex numberExpression
let endingNumberRegex = Regex(numberExpression, RegexOptions.RightToLeft)

let wordToNumber word = 
    match word with
    | "one" -> 1
    | "two" -> 2
    | "three" -> 3
    | "four" -> 4
    | "five" -> 5
    | "six" -> 6
    | "seven" -> 7
    | "eight" -> 8
    | "nine" -> 9
    | _ -> word |> int

let getMatchValue (matchResult: Match) = matchResult.Value

let firstLastNumbers (letters: string) = 
    let first = startingNumberRegex.Match letters |> getMatchValue |> wordToNumber
    let last = endingNumberRegex.Match letters |> getMatchValue |> wordToNumber
    (first, last)

let partBExample = [
    "two1nine";
    "eightwothree";
    "abcone2threexyz";
    "xtwone3four";
    "4nineeightseven2";
    "zoneight234";
    "7pqrstsixteen]"]

let partB (lines: seq<string>) =
    calibrate lines firstLastNumbers

partB partBExample
partB lines