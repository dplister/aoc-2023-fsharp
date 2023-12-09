open System
open System.Text.RegularExpressions

// --- Part A ---

let numberExpression = Regex "([\-0-9]+)"

let parseNumbers (input: string) =
    numberExpression.Matches input
    |> Seq.map (fun n -> n.Value |> int)
    |> List.ofSeq

let example = [
    "0 3 6 9 12 15"
    "1 3 6 10 15 21"
    "10 13 16 21 30 45"
]

[0;3;6;9;12;15] = parseNumbers example[0]

let differences (nums: int list) =
    let rec loop (ls: int list) (acc: int list) =
        if ls.Length < 2 then
            List.rev acc
        else
            loop ls.Tail ((ls[1] - ls[0]) :: acc)
    loop nums []

[3;3;3;3;3] = differences [0;3;6;9;12;15]
[2;3;4;5;6;7] = differences [1;3;6;10;15;21;28]

let differenceSequences (nums: int list) =
    let rec loop (acc: int list list) =
        if List.forall (fun n -> n = 0) acc[0] then 
            acc.Tail
        else 
            loop ((differences acc[0]) :: acc)
    loop [nums]

differenceSequences [10;13;16;21;30;45]

let extrapolate (nums: int list list) =
    let flippedNums = nums |> List.map List.rev
    ([0], flippedNums) ||> Seq.fold (fun acc ls -> 
        (acc[0] + ls.Head) :: acc
    )

extrapolate [
    [2;2;2]
    [0;2;4;6]
    [3;3;5;9;15]
    [10;13;16;21;30;45]
]

let partA (input: string seq) =
    input 
    |> Seq.map (fun line ->
        parseNumbers line
        |> differenceSequences
        |> extrapolate
        |> List.head
    )
    |> Seq.sum

partA example

let lines = (IO.File.ReadAllLines "day09inp.txt") |> List.ofSeq
partA lines

// --- Part B ---

let infer (nums: int list list) =
    ([0], nums) ||> Seq.fold (fun acc ls -> 
        (ls.Head - acc[0]) :: acc
    )

let inferSequence (line: string) =
    parseNumbers line
    |> differenceSequences
    |> infer
    |> List.head

inferSequence "10  13  16  21  30  45"

let partB (input: string seq) =
    input 
    |> Seq.map inferSequence
    |> Seq.sum

partB example

partB lines