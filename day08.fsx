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

let nextLocation (nav: Map<string, (string * string)>) (steps: char array) location index =
    if steps[index] = 'L' then 
        (fst nav[location]) 
    else 
        (snd nav[location])

let nextStep (steps: char array) (currentStep: int) =
        if currentStep + 1 >= steps.Length then 0 else currentStep + 1

let navigate (nav: Map<string, (string * string)>) (steps: char array) termCondition startingLocation startingIndex =
    let rec walk (location: string) (currentStep: int) (counter: int) =
        if termCondition location then
            (location, currentStep, counter)
        else
            walk (nextLocation nav steps location currentStep) (nextStep steps currentStep) (counter + 1)
    walk startingLocation startingIndex 0

let partA (lines: string list) =
    let steps = lines[0] |> Seq.toArray
    let nav = parseNavigation (List.skip 2 lines)
    let (_, _, endCounter) = navigate nav steps (fun loc -> loc = "ZZZ") "AAA" 0
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

let findCycle (nav: Map<string, string * string>) steps termCondition startingLocation =
    let (endLoc, stepsIndex, endCounter) = navigate nav steps termCondition startingLocation 0
    // from this point, continue loop
    printfn "FIRST: location: %A stepsIndex: %A counter %A" endLoc stepsIndex endCounter
    let (finalLoc, finalIndex, finalCounter) = navigate nav steps termCondition (nextLocation nav steps endLoc stepsIndex) (nextStep steps stepsIndex)
    printfn "SECOND: location: %A stepsIndex: %A counter %A" finalLoc finalIndex (finalCounter + 1)
    finalCounter + 1

let partB (lines: string list) =
    let steps = lines[0] |> Seq.toArray
    let nav = parseNavigation (List.skip 2 lines)
    let cycles = 
        allPositionsEndingInA nav
        |> List.map (fun loc -> 
            findCycle nav steps (fun l -> endsIn 'Z' l) loc
        )
    List.fold (fun acc v -> lcm acc v) cycles.Head cycles.Tail

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