open System
open System.Text.RegularExpressions

// --- Part A ---

let seedExpression = Regex "seeds: (.+)"
let mapExpression = Regex "([a-z]+)-to-([a-z]+) map:"
let numberExpression = Regex "([0-9]+)"
let emptyExpression = Regex "^\s*$"

type NumRange = {
    Start: uint64;
    End: uint64;
}

type Almanac = {
    Source: NumRange
    Target: NumRange
    Span: uint64
}

type Section = {
    Source: string
    Target: string
    Ranges: Almanac list
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
            |> Seq.map (fun ns -> { 
                Source = { Start = ns[1]; End = ns[1] + ns[2] - 1UL}
                Target = { Start = ns[0]; End = ns[0] + ns[2] - 1UL}
                Span = ns[2] 
            })
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
    |> List.tryPick (fun (r: Almanac) -> 
        if value >= r.Source.Start && value <= r.Source.End then 
            Some(r.Target.Start + (value - r.Source.Start))
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

// --- Part B ---

let intersect (pxs, pse) (txs, txe) =
    txs <= pse && txe >= pxs


let splitIfIntersects ((sx, sy): uint64 * uint64) ((tx, ty): uint64 * uint64) = 
    // range surrounds the almanac
    if tx > sx && ty < sy then
        // left (p) right (a) middle (p)
        [(sx, tx - 1UL); (tx, ty); (ty + 1UL, sy)]
    // almanac starts before range and ends inside range
    else if tx < sx && ty >= sx && ty <= sy then
        // left (a) right (p)
        [(sx, ty); (ty + 1UL, sy)]
    // almanac starts inside range and ends outside it
    else if tx > sx && tx <= sy && ty >= sy then
        // left (p) right (a)
        [(sx, tx - 1UL); (tx, sy)]
    // either almanac encompasses range or completely misses it
    else
        [(sx,sy)]

[(1UL, 1UL); (2UL, 3UL); (4UL, 4UL)] = splitIfIntersects (1UL, 4UL) (2UL, 3UL)
[(2UL, 3UL); (4UL, 4UL)] = splitIfIntersects (2UL, 4UL) (1UL, 3UL)
[(2UL, 2UL); (3UL, 4UL)] = splitIfIntersects (2UL, 4UL) (3UL, 5UL)
[(2UL, 2UL); (3UL, 4UL)] = splitIfIntersects (2UL, 4UL) (3UL, 4UL)
[(2UL, 3UL); (4UL, 4UL)] = splitIfIntersects (2UL, 4UL) (4UL, 5UL)
[(1UL, 1UL); (2UL, 3UL)] = splitIfIntersects (1UL, 3UL) (0UL, 1UL)

let rec splitOnAlmanac (almanac: Almanac) (currentRanges: (uint64 * uint64) list) (acc: (uint64 * uint64) list) =
    match currentRanges with
    | head :: tail -> 
        splitOnAlmanac almanac tail (List.append (splitIfIntersects head (almanac.Source.Start, almanac.Source.End)) acc)
    | [] -> acc

// splits the current range across all the almanac ranges and then performs the almanac target mapping
let rec splitRangesOnAlmanacs (currentRanges: (uint64 * uint64) list) (almanacRanges: Almanac list) =
    match almanacRanges with
    | head :: tail -> 
        splitRangesOnAlmanacs (splitOnAlmanac head currentRanges []) tail
    | _ -> 
        currentRanges

let translateIfFound (almanacs: Almanac list) ((crx, cry): uint64 * uint64) =
    almanacs |> List.tryPick (fun ma -> 
        if intersect (crx, cry) (ma.Source.Start, ma.Source.End) then 
            // convert the points to the almanac
            let offset = crx - ma.Source.Start
            Some(ma.Target.Start + offset, ma.Target.Start + offset + (cry - crx))
        else 
            None
        )

intersect (79UL, 93UL) (52UL, 100UL)
Some(77UL, 91UL) = 
    translateIfFound 
        [{ Source = { Start = 52UL; End = 100UL }; Target = { Start = 50UL; End = 98UL }; Span = 48UL }]
        (79UL, 93UL)

let translateRangesToDestinations (almanacs: Almanac list) (currentRanges: (uint64 * uint64) list) =
    currentRanges
    |> List.map (fun (cr: (uint64 * uint64)) -> 
        let matchingAlmanac = translateIfFound almanacs cr
        match matchingAlmanac with
        | Some ma -> ma
        | None -> cr
    )

let rec navigateRange (sections: Map<string, Section>) (currentSection: string) (currentRanges: (uint64 * uint64) list) =
    let section = sections.TryFind currentSection
    match section with
    | Some s -> 
        let splitRanges = splitRangesOnAlmanacs currentRanges s.Ranges
        let translated = translateRangesToDestinations s.Ranges splitRanges
        navigateRange sections s.Target translated
    | None -> 
        currentRanges

let pair ls =
    let rec loop ls acc =
        match ls with
        | head1 :: head2 :: tail ->
            loop tail ((head1,head2) :: acc)
        | _ ->
            List.rev acc
    loop ls []

let partB (lines: string list) =
    let texts = splitText lines
    let seeds = 
        readSeeds texts.Head.Head
        |> List.ofSeq
        |> pair
        |> List.map (fun (p1, p2) -> 
            (p1, p1 + (p2 - 1UL))
        )
    let sections = mapSections texts.Tail
    navigateRange sections "seed" seeds
        |> List.minBy (fun (px, py) -> px)

partB lines
