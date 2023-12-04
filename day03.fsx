open System

// --- Part A ---

type Position = {
    X: int
    Y: int
    Content: string
    IsNumber: bool
}

// converts the line of items into a sequence of numbers and symbols with their offset
let readLine (input: char list) =
    let addToAcc (buffer: char list) (isNumber: bool) (index: int) acc =
        if buffer.IsEmpty then 
            acc
        else
            { X = (index - buffer.Length); IsNumber = isNumber; Content = (Seq.rev buffer |> String.Concat); Y = 0 } :: acc
    let rec loop (working: char list) (buffer: char list) (index: int) acc =
        match working with
        | head :: tail ->
            match head with
            | '.' -> 
                loop tail [] (index + 1) (addToAcc buffer true index acc)
            | c when Char.IsDigit c -> 
                loop tail (c :: buffer) (index + 1) acc
            | c ->
                loop tail [] (index + 1) 
                    // for cases where symbol is directly after a number add buffer
                    (addToAcc buffer true index 
                        (addToAcc [c] false (index + 1) acc))
        | [] -> addToAcc buffer true index acc
    loop input [] 0 []

let readPositions (input: string seq) =
    Seq.map2 (fun (y: int) (line: string) -> 
        let ps = readLine (line |> List.ofSeq)
        ps |> List.map (fun p -> { p with Y = y })
    ) [0..(Seq.length input)] input
    |> List.concat

let example = [
    "467..114..";
    "...*......";
    "..35..633.";
    "......#...";
    "617*......";
    ".....+.58.";
    "..592.....";
    "......755.";
    "...$.*....";
    ".664.598.."
]

// check if points are present
readPositions example

// returns the x coordinate of where the position starts and ends
let numberDimensions position =
    (position.X, position.X + (position.Content.Length - 1))

let allNearY y (positions: Position list) =
    positions |> List.filter (fun p -> p.Y >= y - 1 && p.Y <= y + 1)

let allBetweenX (startX, endX) (positions: Position list) =
    positions |> List.filter (fun p -> 
        let (sx, ex) = numberDimensions p
        sx >= (startX - 1) && sx <= (endX + 1)
            || ex >= (startX - 1) && sx <= (endX + 1))

let hasSymbol (positions: Position list) =
    positions |> List.filter (fun p -> not p.IsNumber)

let hasNumber (positions: Position list) =
    positions |> List.filter (fun p -> p.IsNumber)

// looking for Content = 755, X = 6, Y = 7
readPositions example
|> allNearY 7
|> allBetweenX (5, 7)
|> hasSymbol

// finds all points near position
let near position points =
    points 
    |> allNearY position.Y
    |> allBetweenX (numberDimensions position)

let validPositions (lines: seq<string>) = 
    let positions = readPositions lines
    let symbols = positions |> hasSymbol
    positions
        |> hasNumber
        |> List.filter (fun p -> 
            (near p symbols) |> Seq.isEmpty |> not
        )

let partA (lines: seq<string>) =
    validPositions lines
        |> Seq.map (fun (p: Position) -> p.Content |> int)
        |> Seq.sum

let lines = (IO.File.ReadAllLines "day03inp.txt")
partA lines

// -- Part B ---

let validCogs (lines: seq<string>) =
    let positions = readPositions lines
    let numbers = positions |> hasNumber
    positions 
        |> List.filter (fun p -> p.Content = "*")
        |> List.map (fun p -> 
            (p, near p numbers)
        )
        |> List.map (fun (p, ns) -> 
            if ns.Length = 2 then
                (ns[0].Content |> int) * (ns[1].Content |> int)
            else
                0)
        |> List.sum

validCogs example

validCogs lines

// TODO:
// union type for position
// store dimensions in type