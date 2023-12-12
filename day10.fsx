open System

// --- Part A ---

let example = [
    "....."
    ".S-7."
    ".|.|."
    ".L-J."
    "....."
]

type Direction = West | East | North | South

type Pos = {
    X: int
    Y: int
    Tile: char
    mutable Directions: Direction list
    mutable Score: int
    IsExtension: bool
}

let letterToDirections c =
    match c with
    | '|' -> [ Direction.North; Direction.South ]
    | '-' -> [ Direction.West; Direction.East ]
    | 'L' -> [ Direction.North; Direction.East ]
    | 'J' -> [ Direction.North; Direction.West ]
    | '7' -> [ Direction.South; Direction.West ]
    | 'F' -> [ Direction.South; Direction.East ]
    | _ -> []

let inverseDirection dir =
    match dir with
    | Direction.West -> Direction.East
    | Direction.East -> Direction.West
    | Direction.South -> Direction.North
    | Direction.North -> Direction.South

let createPos x y c = 
    { 
        X = x
        Y = y
        Tile = c
        Directions = letterToDirections c
        Score = -1
        IsExtension = false
    }

let parseMap (input: string list) = 
    let arr = Array2D.create input.Length input[0].Length (createPos 0 0 '.')
    (arr, [0..(input.Length - 1)], input) |||> Seq.fold2 (fun a y l -> 
        let lineArr = l |> Seq.toArray
        for x in 0 .. (l.Length - 1) do
            a[y, x] <- createPos y x lineArr[x]
        a
    )

let printArray accessor (overview: Pos array2d) =
    let width = (Array2D.length2 overview) - 1
    overview |> Array2D.iteri (fun y x v -> 
        printf "%A" (accessor v)
        if x = width then 
            printf "\n"
    )

parseMap example
|> printArray (fun v -> v.Tile)

let getDirection (overview: Pos array2d) y x dir =
    match dir with 
    | Direction.North -> 
        let ny = y - 1
        if ny < 0 then 
            None
        else 
            Some(overview[ny, x])
    | Direction.South ->
        let ny = y + 1
        if ny >= (overview |> Array2D.length1) then
            None
        else
            Some(overview[ny, x])
    | Direction.West ->
        let nx = x - 1
        if nx < 0 then
            None
        else
            Some(overview[y, nx])
    | Direction.East ->
        let nx = x + 1
        if nx >= (overview |> Array2D.length2) then
            None
        else
            Some(overview[y, nx])

let getAdjacent (overview: Pos array2d) y x directions =
    directions 
    |> List.map (fun d -> (d, getDirection overview y x d))
    |> List.choose (fun (d, v) -> if v.IsSome then Some(d, v.Value) else None)

// finds item within array based on predicate
// naive implementation that expects to find element
let findItem predicate (overview: Pos array2d) =
    let width = overview |> Array2D.length2
    let rec loop y x =
        if x >= width then 
            loop (y + 1) 0
        else if predicate overview[y,x] then
            overview[y,x]
        else 
            loop y (x + 1)
    loop 0 0

parseMap example
|> findItem (fun p -> p.Tile = 'S')

let findConnectionsToItem (overview: Pos array2d) (item: Pos) =
    getAdjacent overview item.Y item.X [Direction.North;Direction.South;Direction.East;Direction.West]
    // from all directions, work out which point back to the start
    |> List.choose (fun (d, p) -> 
        let invDir = (inverseDirection d)
        let res = p.Directions |> List.tryFind (fun cd -> invDir = cd)
        match res with 
        | Some _ -> Some(d)
        | None -> None
    )

let partA (input: string list) =
    let overview = parseMap example
    let item = findItem (fun p -> p.Tile = 'S') overview
    let dirs = findConnectionsToItem overview item
    item.Directions <- dirs

// --- Part B ---

let extendMap (overview: Pos array2d) =
    let oy = (overview |> Array2D.length1)
    let ox = (overview |> Array2D.length2)
    let my = oy * 2 - 1
    let mx = ox * 2 - 1
    let arr = 
        Array2D.create 
            my
            mx
            (createPos 0 0 '.')
    // for each row, extend below
    let mutable cx = 0
    let mutable cy = 0
    for y in 0 .. (oy - 1) do
        for x in 0 .. (ox - 2) do
            let nx = cx + 1
            let ny = cy + 1
            arr[cy, cx] <- overview[y,x]
            // provide cell underneath
            if ny < (my - 1) then 
                arr[ny, cx] <- { (createPos cx ny (if overview[y,x].Directions |> List.exists (fun d -> d = South) then '|' else '.')) with IsExtension = true }
            // provide cell to the right
            arr[cy, nx] <- { (createPos nx y (if overview[y,x].Directions |> List.exists (fun d -> d = East) then '-' else '.')) with IsExtension = true }
            // compensate for the diagonal to the bottom right (which is always empty)
            if ny < (my - 1) && nx < (mx - 1) then
                arr[ny, nx] <- { (createPos nx ny '.') with IsExtension = true }
            cx <- cx + 2
        cy <- cy + 2
        cx <- 0
    arr

let partB (input: string list) =
    let overview = parseMap input
    let item = findItem (fun p -> p.Tile = 'S') overview
    let dirs = findConnectionsToItem overview item
    item.Directions <- dirs
    let extended = overview |> extendMap
    printArray (fun p -> p.Tile) extended
    
partB example

let pipeExample = [
    ".........."
    ".S------7."
    ".|F----7|."
    ".||....||."
    ".||....||."
    ".|L-7F-J|."
    ".|..||..|."
    ".L--JL--J."
    ".........."
]

partB pipeExample