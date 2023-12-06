open System
open System.Text.RegularExpressions

let seedExpression = Regex "seeds: (.+)"
let mapExpression = Regex "([a-z]+)-to-([a-z]+) map:"
let numberExpression = Regex "([0-9]+)"
let emptyExpression = Regex "^\s*$"

type NumberRange = {
    Source: uint64
    Target: uint64
    Range: uint64
}

type Section = {
    Source: string
    Target: string
    Ranges: NumberRange list
}

let readSeeds (input: string) =
    let m = seedExpression.Match input
    numberExpression.Matches m.Groups[1].Value
    |> Seq.map (fun mn -> mn.Value |> uint64)

let readSection (input: string list) =
    let header = mapExpression.Match input[0]
    let numberSets = 
        input.Tail 
        |> List.map (fun line -> 
            numberExpression.Matches line
            |> Seq.map (fun m -> m.Value |> uint64)
            |> List.ofSeq
    )
    {
        Source = header.Groups[1].Value
        Target = header.Groups[2].Value
        Ranges = numberSets
            |> Seq.map (fun ns -> { Source = ns[1]; Target = ns[0]; Range = ns[2] })
            |> List.ofSeq
    }

readSection [
    "seed-to-soil map:"
    "50 98 2"
    "52 50 48"]

let splitText (input: string list) =
    let addToSecs (acc: string list) (secs: string list list) =
        if acc.IsEmpty then
            secs 
        else
            acc :: secs
    let rec loop (ls: string list) (acc: string list) (secs: string list list) =
        match ls with
        | head :: tail when (emptyExpression.IsMatch head) ->
            loop tail [] (addToSecs (List.rev acc) secs)
        | head :: tail ->
            loop tail (head :: acc) secs
        | [] ->
            (addToSecs (List.rev acc) secs)
    List.rev (loop input [] [])

[
    ["a";"b";"c"];
    ["d";"e"];
    ["f"]
] = splitText [
    "a";"b";"c";"";"d";"e";"";"";"f"
]

let mapSections (lines: string list list) = 
    (Map.empty, lines) ||> Seq.fold (fun m l -> 
        let section = readSection l
        m.Add (section.Source, section)
    )

let sourceToTarget (section: Section) (value: uint64) =
    section.Ranges
    |> List.tryPick (fun (r: NumberRange) -> 
        if value >= r.Source && value < r.Source + r.Range then 
            Some(r.Target + (value - r.Source))
        else
            None)
    |> Option.defaultValue value

let navigateSections (sections: Map<string, Section>) (origin: string) (startingValue: uint64) =
    let rec loop (current: string) (value: uint64) =
        let section = sections.TryFind current
        match section with
        | Some s -> 
            loop s.Target (sourceToTarget s value)
        | None -> 
            value
    loop origin startingValue

let example = [
    "seeds: 79 14 55 13"
    ""
    "seed-to-soil map:"
    "50 98 2"
    "52 50 48"
    ""
    "soil-to-fertilizer map:"
    "0 15 37"
    "37 52 2"
    "39 0 15"
    ""
    "fertilizer-to-water map:"
    "49 53 8"
    "0 11 42"
    "42 0 7"
    "57 7 4"
    ""
    "water-to-light map:"
    "88 18 7"
    "18 25 70"
    ""
    "light-to-temperature map:"
    "45 77 23"
    "81 45 19"
    "68 64 13"
    ""
    "temperature-to-humidity map:"
    "0 69 1"
    "1 0 69"
    ""
    "humidity-to-location map:"
    "60 56 37"
    "56 93 4"
]

splitText example

let partA (lines: string list) =
    let texts = splitText lines
    let seeds = readSeeds texts.Head.Head
    let sections = mapSections texts.Tail
    seeds
    |> Seq.map (fun s -> navigateSections sections "seed" s)
    |> Seq.min

partA example

let lines = (IO.File.ReadAllLines "day05inp.txt") |> List.ofSeq
partA lines