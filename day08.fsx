open System
open System.Text.RegularExpressions

// --- Part A ---

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

let navExpression = Regex "([0-9A-Z]+) = \(([0-9A-Z]+), ([0-9A-Z]+)\)"

let parseNavigation (lines: string list) = 
    lines
    |> List.map (fun line -> 
        let m = navExpression.Match line
        (m.Groups[1].Value, (m.Groups[2].Value, m.Groups[3].Value)))
    |> Map

let navigate (nav: Map<string, (string * string)>) termCondition (steps: char list) startingLocation =
    let rec walk (location: string) (remainingSteps: char list) (counter: int) =
        if termCondition location then
            (location, remainingSteps, counter)
        else
            match remainingSteps with
            | head :: tail ->
                let next = if head = 'L' then (fst nav[location]) else (snd nav[location])
                walk next tail (counter + 1)
            | [] -> (location, remainingSteps, counter)
    let rec loop (location: string) (counter: int) =
        let (endLoc, remainingSteps, endCounter) = (walk location steps counter)
        if termCondition endLoc then
            (endLoc, remainingSteps, endCounter)
        else 
            loop endLoc endCounter
    loop startingLocation 0

let partA (lines: string list) =
    let steps = lines[0] |> Seq.toList
    let nav = parseNavigation (List.skip 2 lines)
    let (_, _, endCounter) = navigate nav (fun loc -> loc = "ZZZ") steps "AAA"
    endCounter

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

// --- Part B ---

let endsIn (c: char) (input: string) =
    input |> Seq.last = c

endsIn 'Z' "ASZ"

let allPositionsEndingInA (nav: Map<string, (string  * string)>) = 
    nav.Keys 
    |> Seq.filter (endsIn 'A')
    |> List.ofSeq

let rec gcd x y = if y = 0 then abs x else gcd y (x % y)

let lcm x y = x * y / (gcd x y)

let findCycle (nav: Map<string, string * string>) termCondition steps startingLocation =
    let (endLoc, remainingSteps, endCounter) = navigate nav termCondition steps startingLocation
    // from this point, continue loop
    let appendedSteps = 
        List.append 
            remainingSteps
            (List.take (steps.Length - remainingSteps.Length) steps)
    

let partB (lines: string list) =
    let steps = lines[0] |> Seq.toList
    let nav = parseNavigation (List.skip 2 lines)
    let cycles = 
        allPositionsEndingInA nav
        |> List.map (fun loc -> 
            findCycle nav (fun l -> endsIn 'Z' l) steps loc
        )
    cycles
    // List.fold (fun acc v -> lcm acc v) cycles.Head cycles

let complexExample = [
    "LR"
    ""
    "11A = (11B, XXX)"
    "11B = (XXX, 11Z)"
    "11Z = (11B, XXX)"
    "22A = (22B, XXX)"
    "22B = (22C, 22C)"
    "22C = (22Z, 22Z)"
    "22Z = (22B, 22B)"
    "XXX = (XXX, XXX)"
]

partB complexExample

partB lines

// not 846203634

// to implement
let rec x = seq { 
    yield "Hello"
    yield "World"
    yield! ["foo"; "bar"]
    yield! x
}