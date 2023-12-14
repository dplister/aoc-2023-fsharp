open System

// --- Part A ---

type Pos = {
    X: int64
    Y: int64
}

let parseGalaxy (input: string list) =
    ([], [0..(input.Length - 1)], input) |||> Seq.fold2 (fun acc y line -> 
        let galaxysInLine = ([], [0..(line.Length - 1)], line) |||> Seq.fold2 (fun al x c ->
            if c = '#' then
                { X = x; Y = y } :: al
            else
                al
        )
        List.append galaxysInLine acc
    )

let example = [
    "...#......"
    ".......#.."
    "#........."
    ".........."
    "......#..."
    ".#........"
    ".........#"
    ".........."
    ".......#.."
    "#...#....."
]

parseGalaxy example

let getGaps (nums: int64 list) =
    let nums = nums |> List.distinct |> List.sort
    let rec loop (prev: int64) (ls: int64 list) (acc: int64 list) =
        match ls with
        | head :: tail -> 
            if head - prev > 1 then 
                loop head tail (List.append acc [(prev + 1L)..(head - 1L)])
            else
                loop head tail acc
        | [] -> acc
    loop nums[0] nums [0..(nums[0] - 1L)]

getGaps [1L;3L;7L]
getGaps [2L;3L;7L]

let padGaps (multiplier: int64) (ps: Pos list) =
    let xgaps = ps |> List.map (fun p -> p.X) |> getGaps
    let ygaps = ps |> List.map (fun p -> p.Y) |> getGaps
    let multiplier = if multiplier > 1 then multiplier - 1L else multiplier
    ps |> List.map (fun p -> 
    { 
        X = p.X + ((xgaps |> List.filter (fun xg -> xg < p.X) |> List.length |> int64) * multiplier)
        Y = p.Y + ((ygaps |> List.filter (fun yg -> yg < p.Y) |> List.length |> int64) * multiplier)
    })

parseGalaxy example
|> padGaps 1

// generates unique pairs from list
let pairs ls =
    let rec loop ls =
        match ls with
        | head :: tail -> 
            List.append
                (tail |> List.map (fun (p) -> [head; p]))
                (loop tail)
        | [] -> []
    loop ls

pairs [1;2;3]
parseGalaxy example |> pairs |> List.length

type Axis = X | Y

// move the number p closer to t by 1
let closerOne p t = 
    if p < t then 
        p + 1
    else if p > t then
        p - 1
    else 
        p

let distanceBetween p1 p2 = 
    let minX, maxX = min p1.X p2.X, max p1.X p2.X
    let minY, maxY = min p1.Y p2.Y, max p1.Y p2.Y
    (maxX - minX) + (maxY - minY)

9L = distanceBetween { X = 1; Y = 6 } { X = 5; Y = 11 }
15L = distanceBetween { X = 4; Y = 0 } { X = 9; Y = 10 }
17L = distanceBetween { X = 0; Y = 2 } { X = 12; Y = 7 }
5L = distanceBetween { X = 0; Y = 11 } { X = 5; Y = 11 }

let calculateDistances (ps: Pos list list) =
    ps |> List.map (fun pp -> (pp, (distanceBetween pp[0] pp[1])))

let partA (lines: string list) =
    lines
    |> parseGalaxy
    |> padGaps 1
    |> pairs
    |> calculateDistances
    |> List.sumBy (fun (_, d) -> d)

partA example

let lines = (IO.File.ReadAllLines "day11inp.txt") |> List.ofSeq
partA lines

// --- Part B ---

let partB multiplier (lines: string list) =
    lines
    |> parseGalaxy
    |> padGaps multiplier
    |> pairs
    |> calculateDistances
    |> List.sumBy (fun (_, d) -> d)

printfn "Part B: %A" (partB 1000000L lines)