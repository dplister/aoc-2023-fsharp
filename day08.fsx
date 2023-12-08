open System
open System.Text.RegularExpressions

let example = [
    "RL"
    ""
    "AAA = (BBB, CCC)"
    "BBB = (DDD, EEE)"
    "CCC = (ZZZ, GGG)"
    "DDD = (DDD, DDD)"
    "EEE = (EEE, EEE)"
    "GGG = (GGG, GGG)"
    "ZZZ = (ZZZ, ZZZ)"
]

let navExpression = Regex "([A-Z]+) = \(([A-Z]+), ([A-Z]+)\)"

let parseNavigation (lines: string list) = 
    lines
    |> List.map (fun line -> 
        let m = navExpression.Match line
        (m.Groups[1].Value, (m.Groups[2].Value, m.Groups[3].Value)))
    |> Map

let navigate (nav: Map<string, (string * string)>) (steps: char list) =
    let rec walk (location: string) (remainingSteps: char list) (counter: int) =
        if location = "ZZZ" then
            (location, counter)
        else
            match remainingSteps with
            | head :: tail ->
                let next = if head = 'L' then (fst nav[location]) else (snd nav[location])
                walk next tail (counter + 1)
            | [] -> (location, counter)
    let rec loop (location: string) (counter: int) =
        match (walk location steps counter) with
        | "ZZZ", i -> i
        | loc, i -> loop loc i
    loop "AAA" 0

let partA (lines: string list) =
    let steps = lines[0] |> Seq.toList
    let nav = parseNavigation (List.skip 2 lines)
    navigate nav steps

2 = partA example

let repeatExample = [
    "LLR"
    ""
    "AAA = (BBB, BBB)"
    "BBB = (AAA, ZZZ)"
    "ZZZ = (ZZZ, ZZZ)"
]

6 = partA repeatExample

let lines = (IO.File.ReadAllLines "day08inp.txt") |> List.ofSeq
partA lines