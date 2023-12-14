open System

type Pos = {
    X: int
    Y: int
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

let getGaps (nums: int list) =
    let nums = nums |> List.distinct |> List.sort
    let rec loop (prev: int) (ls: int list) (acc: int list) =
        match ls with
        | head :: tail -> 
            if head - prev > 1 then 
                loop head tail (List.append acc [(prev + 1)..(head - 1)])
            else
                loop head tail acc
        | [] -> acc
    loop nums[0] nums [0..(nums[0] - 1)]

getGaps [1;3;7]
getGaps [2;3;7]

let padGaps (ps: Pos list) =
    let xgaps = ps |> List.map (fun p -> p.X) |> getGaps
    let ygaps = ps |> List.map (fun p -> p.Y) |> getGaps
    ps |> List.map (fun p -> 
    { 
        X = p.X + (xgaps |> List.filter (fun xg -> xg < p.X) |> List.length)
        Y = p.Y + (ygaps |> List.filter (fun yg -> yg < p.Y) |> List.length)
    })

parseGalaxy example
|> padGaps

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
    let rec loop x y alt counter =
        if x = p2.X && y = p2.Y then
            counter
        else if (alt = X && x <> p2.X) || (alt = Y && y = p2.Y) then 
            loop (closerOne x p2.X) y Y (counter + 1)
        else 
            loop x (closerOne y p2.Y) X (counter + 1)
    loop p1.X p1.Y X 0

9 = distanceBetween { X = 1; Y = 6 } { X = 5; Y = 11 }
15 = distanceBetween { X = 4; Y = 0 } { X = 9; Y = 10 }
17 = distanceBetween { X = 0; Y = 2 } { X = 12; Y = 7 }
5 = distanceBetween { X = 0; Y = 11 } { X = 5; Y = 11 }

let calculateDistances (ps: Pos list list) =
    ps |> List.map (fun pp -> (pp, (distanceBetween pp[0] pp[1])))

let partA (lines: string list) =
    lines
    |> parseGalaxy
    |> padGaps
    |> pairs
    |> calculateDistances
    |> List.sumBy (fun (_, d) -> d)

// partA example

let lines = (IO.File.ReadAllLines "day11inp.txt") |> List.ofSeq
partA lines