open System

// --- Part A ---

let example = [
    "O....#...."
    "O.OO#....#"
    ".....##..."
    "OO.#O....O"
    ".O.....O#."
    "O.#..O.#.#"
    "..O..#O..O"
    ".......O.."
    "#....###.."
    "#OO..#...."
]

type Movable = Fixed | Movable

type Pos = {
    mutable X: int 
    mutable Y: int
}

type Item = {
    Pos: Pos
    Movable: Movable
}

let createItem y x c =
    {
        Pos = { X = x; Y = y; }
        Movable = 
            match c with 
            | '#' -> Fixed
            | 'O' -> Movable
            | _ -> failwith $"Unknown character: {c}"
    }

let parseItems (input: string list) =
    input |> List.mapi (fun y l -> 
        l |> Seq.mapi (fun x c ->
            if c = '#' || c = 'O' then 
                Some (createItem y x c)
            else
                None
        )
        |> List.ofSeq |> List.choose id
    )
    |> List.filter (fun l -> l.Length > 0)
    |> List.reduce List.append

parseItems example

/// finds the highest value based on the accessor applied to each item
let maxItem (items: 'a list) accessor =
    items |> List.maxBy accessor |> accessor

/// Shifts all items to their lowest y based on Fixed item obstruction
let shiftItems (items: Item list) =
    let maxX = maxItem items (fun p -> p.Pos.X)
    let rec loop (x: int) =
        if x <= maxX then 
            let column = items |> List.filter (fun p -> p.Pos.X = x)
            let rec inner (y: int) (cls: Item list) =
                match cls with 
                | head :: tail -> 
                    if head.Movable = Movable then 
                        head.Pos.Y <- y
                        inner (y + 1) tail
                    else 
                        inner (head.Pos.Y + 1) tail
                | [] -> ()
            inner 0 column
            loop (x + 1)
    loop 0
    items

/// Renders the set of items to resemble the raw input
let drawItems (items: Item list) =
    let maxX = maxItem items (fun p -> p.Pos.X)
    let maxY = maxItem items (fun p -> p.Pos.Y)
    for y in [0..maxY] do
        for x in [0..maxX] do
            let item = items |> List.tryFind (fun p -> p.Pos.X = x && p.Pos.Y = y)
            let c = 
                match item with 
                | Some it -> if it.Movable = Fixed then '#' else 'O'
                | None -> '.'
            printf "%c" c
            if x = maxX then
                printf "\n"
    items

// example
// |> parseItems
// |> shiftItems
// |> drawItems 

/// Measures the amount of weight of each item by distance from bottom
/// <Remarks> Achieves this by inverting the Y axis </Remarks>
let scoreWeight (items: Item list) =
    let maxY = maxItem items (fun p -> p.Pos.Y)
    items 
    |> List.map (fun it -> if it.Movable = Fixed then 0 else (maxY - it.Pos.Y) + 1)
    |> List.sum

let partA (input: string list) =
    input
    |> parseItems
    |> shiftItems
    |> drawItems
    |> scoreWeight

// partA example

let lines = (IO.File.ReadAllLines "day14inp.txt") |> List.ofSeq
// partA lines

// --- Part B ---

let shiftItemsAxis primaryAxis secondaryAxis secondaryAxisUpdate starting increment (items: Item list) =
    let maxAxis = maxItem items primaryAxis
    let startingValue = (starting 0 maxAxis)
    let rec loop (n: int) =
        if n <= maxAxis then 
            let matchingSet = items |> List.filter (fun p -> primaryAxis p = n) 
            let orderedSet = 
                if startingValue = 0 then 
                    matchingSet |> List.sortBy (fun p -> secondaryAxis p)
                else
                    matchingSet |> List.sortByDescending (fun p -> secondaryAxis p)
            let rec inner (sax: int) (cls: Item list) =
                match cls with 
                | head :: tail -> 
                    if head.Movable = Movable then 
                        secondaryAxisUpdate head sax
                        inner (increment sax) tail
                    else 
                        inner (increment (secondaryAxis head)) tail
                | [] -> ()
            inner startingValue orderedSet
            loop (n + 1)
    loop 0
    items

let shiftNorth (items: Item list) =
    items |> shiftItemsAxis (fun p -> p.Pos.X) (fun p -> p.Pos.Y) (fun p v -> p.Pos.Y <- v) (fun mn _ -> mn) (fun v -> v + 1)

let shiftSouth (items: Item list) =
    items |> shiftItemsAxis (fun p -> p.Pos.X) (fun p -> p.Pos.Y) (fun p v -> p.Pos.Y <- v) (fun _ mx -> mx) (fun v -> v - 1)

let shiftWest (items: Item list) =
    items |> shiftItemsAxis (fun p -> p.Pos.Y) (fun p -> p.Pos.X) (fun p v -> p.Pos.X <- v) (fun mn _ -> mn) (fun v -> v + 1)

let shiftEast (items: Item list) =
    items |> shiftItemsAxis (fun p -> p.Pos.Y) (fun p -> p.Pos.X) (fun p v -> p.Pos.X <- v) (fun _ mx -> mx) (fun v -> v - 1)

let rotate (items: Item list) =
    items
    |> shiftNorth
    |> shiftWest
    |> shiftSouth
    |> shiftEast

let rec rotations (n: int) (items: Item list) =
    if n = 0 then 
        items
    else 
        rotations (n - 1) (rotate items)

let partB (input: string list) = 
    input
    |> parseItems
    |> rotations 1000000000
    |> drawItems
    |> scoreWeight

partB example
