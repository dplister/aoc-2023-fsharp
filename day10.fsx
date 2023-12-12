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
    | '|' -> [ North; South ]
    | '-' -> [ West; East ]
    | 'L' -> [ North; East ]
    | 'J' -> [ North; West ]
    | '7' -> [ South; West ]
    | 'F' -> [ South; East ]
    | _ -> []

let inverseDirection dir =
    match dir with
    | West -> East
    | East -> West
    | South -> North
    | North -> South

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
            a[y, x] <- createPos x y lineArr[x]
        a
    )

let printArray accessor (overview: Pos array2d) =
    let width = (Array2D.length2 overview) - 1
    overview |> Array2D.iteri (fun _ x v -> 
        printf "%4s" (accessor v)
        if x = width then 
            printf "\n"
    )

parseMap example
|> printArray (fun v -> v.Tile |> string)

let getDirection (overview: Pos array2d) y x dir =
    match dir with 
    | North -> 
        let ny = y - 1
        if ny < 0 then 
            None
        else 
            Some(overview[ny, x])
    | South ->
        let ny = y + 1
        if ny >= (overview |> Array2D.length1) then
            None
        else
            Some(overview[ny, x])
    | West ->
        let nx = x - 1
        if nx < 0 then
            None
        else
            Some(overview[y, nx])
    | East ->
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
    getAdjacent overview item.Y item.X [North;South;East;West]
    // from all directions, work out which point back to the start
    |> List.choose (fun (d, p) -> 
        let invDir = (inverseDirection d)
        let res = p.Directions |> List.tryFind (fun cd -> invDir = cd)
        match res with 
        | Some _ -> Some(d)
        | None -> None
    )

let connectStart (overview: Pos array2d) =
    let item = findItem (fun p -> p.Tile = 'S') overview
    let dirs = findConnectionsToItem overview item
    item.Directions <- dirs
    item

let navigate (overview: Pos array2d) (start: Pos) =
    let rec loop (ps: Pos list) (counter: int) =
        let next = 
            ps
            |> List.map (fun p -> 
                getAdjacent overview p.Y p.X p.Directions 
                |> List.filter (fun (d, a) -> a.Score = -1))
            |> List.reduce List.append
            |> List.map (fun (_, a) -> a)
        if next.Length = 0 then 
            ps.Head
        else
            next |> List.iter (fun a -> a.Score <- counter)
            loop next (counter + 1)
    start.Score <- 0
    loop [start] 1

let partA (input: string list) =
    let overview = parseMap input
    let start = connectStart overview
    // printArray (fun a -> a.Tile) overview
    // getAdjacent overview start.Y start.X start.Directions
    let result = navigate overview start
    // printArray (fun a -> if a.Score < 0 then "." else a.Score |> string) overview
    result

partA example

let complexExample = [
    "..F7."
    ".FJ|."
    "SJ.L7"
    "|F--J"
    "LJ..."
]

partA complexExample

let lines = (IO.File.ReadAllLines "day10inp.txt") |> List.ofSeq
// partA lines

// --- Part B ---

let hasDirection (pos: Pos) dir =
    pos.Directions |> List.exists (fun d -> d = dir)

let getVerticalConnection (overview: Pos array2d) y x =
    if hasDirection overview[y,x] South
        && ((y + 1) >= (overview |> Array2D.length1)
            || hasDirection overview[y + 1, x] North)
        then
            '|'
        else
            '.'

let getHorizontalConnection (overview: Pos array2d) y x =
    if hasDirection overview[y,x] East 
        && ((x + 1) >= (overview |> Array2D.length2)
            || hasDirection overview[y, x + 1] West)
        then
            '-'
        else
            '.'

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
                arr[ny, cx] <- { (createPos cx ny (getVerticalConnection overview y x)) with IsExtension = true }
            // provide cell to the right
            arr[cy, nx] <- { (createPos nx y (getHorizontalConnection overview y x)) with IsExtension = true }
            // compensate for the diagonal to the bottom right (which is always empty)
            if ny < (my - 1) && nx < (mx - 1) then
                arr[ny, nx] <- { (createPos nx ny '.') with IsExtension = true }
            cx <- cx + 2
        cy <- cy + 2
        cx <- 0
    arr

let floodArea (overview: Pos array2d) (pos: Pos) group =
    if pos.X = 0 && pos.Y = 0 then
        printfn "%A" (getAdjacent overview 2 0 [North;South;East;West])
    let rec loop (positions: Pos list) =
        positions |> List.iter (fun p -> p.Score <- group)
        let next = 
            positions
            |> List.map (fun p ->
                getAdjacent overview p.Y p.X [North;South;East;West] 
                |> List.map (fun (_, np) -> np)
                |> List.filter (fun np -> np.Score = -1 && np.Tile = '.')
            )
            |> List.reduce List.append
        // printfn "%A" next
        if next.Length > 0 then 
            loop next
        else
            ()
    loop [pos]

let floodAreas (overview: Pos array2d) =
    // search through all areas and flood them
    let my = (overview |> Array2D.length1) - 1
    let mx = (overview |> Array2D.length2) - 1
    let mutable group = 1
    for y in 0 .. my do
        for x in 0 .. mx do 
            if overview[y,x].Tile = '.' && overview[y,x].Score = -1 then 
                floodArea overview overview[y,x] group
                group <- group + 1
    ()

let partB (input: string list) =
    let overview = parseMap input
    let item = findItem (fun p -> p.Tile = 'S') overview
    let dirs = findConnectionsToItem overview item
    item.Directions <- dirs
    let extended = overview |> extendMap
    floodAreas extended
    printArray (fun p -> if p.Score > -1 then p.Score |> string else p.Tile |> string) extended
    
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