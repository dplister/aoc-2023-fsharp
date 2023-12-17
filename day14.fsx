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

example
|> parseItems
|> shiftItems
|> drawItems 

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

partA example

let lines = (IO.File.ReadAllLines "day14inp.txt") |> List.ofSeq
partA lines

// --- Part B ---

let shiftItemsAxis primaryAxis secondaryAxis secondaryAxisUpdate (items: Item list) =
    let maxAxis = maxItem items primaryAxis
    let rec loop (n: int) =
        if n >= 0 && n <= maxAxis then 
            let matchingSet = items |> List.filter (fun p -> primaryAxis p = n)
            let rec inner (sax: int) (cls: Item list) =
                match cls with 
                | head :: tail -> 
                    if head.Movable = Movable then 
                        secondaryAxisUpdate head sax
                        inner (sax + 1) tail
                    else 
                        inner (secondaryAxis head + 1) tail
                | [] -> ()
            inner 0 matchingSet
            loop (n + 1)
    loop 0
    items

let partB (input: string list) = 
    input
    |> parseItems
    |> shiftItemsAxis (fun p -> p.Pos.X) (fun p -> p.Pos.Y) (fun p v -> p.Pos.Y <- v)
    |> drawItems

partB example
