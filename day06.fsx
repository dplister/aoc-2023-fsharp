open System
open System.Text.RegularExpressions

// --- Part A ---

let numberExpression = Regex "([0-9]+)"

let parseNumbersFromLine input =
    numberExpression.Matches input 
    |> Seq.map (fun i -> i.Value |> int64)

let example = [
    "Time:      7  15   30"
    "Distance:  9  40  200"
]

let parseTrials (input: string list) =
    Seq.zip 
        (parseNumbersFromLine input[0])
        (parseNumbersFromLine input[1])

parseTrials example

// determines if the combination of time and 
let travel (holdTime: int64) (time: int64) (distance: int64) =
    (time - holdTime) * holdTime

0L = travel 0L 7L 9L
6L = travel 1L 7L 9L
10L = travel 2L 7L 9L
12L = travel 3L 7L 9L

let minMaxTrials ((time, distance): int64 * int64) =
    let findTravel = Seq.find (fun h -> (travel h time distance) > distance)
    // find the minimum to succeed
    let minHold = 
        seq { 1L..time }
        |> findTravel
    // find the maximum to succeed
    let maxHold =
        seq { time..(-1L)..1L }
        |> findTravel
    (minHold, maxHold)

(4L, 11L) = minMaxTrials (15L, 40L)
(11L, 19L) = minMaxTrials (30L, 200L)
    
let partA (input: string list) =
    parseTrials input
    |> Seq.map minMaxTrials
    |> Seq.map (fun (minHold, maxHold) -> (maxHold - minHold) + 1L)
    |> Seq.reduce (*)

partA example

let lines = (IO.File.ReadAllLines "day06inp.txt") |> List.ofSeq
partA lines

// --- Part B ---

let parseSpacedNumberFromLine (input: string) =
    numberExpression.Matches input 
    |> Seq.map (fun m -> m.Value)
    |> String.concat ""
    |> int64

123456L = parseSpacedNumberFromLine "12 34   56"

let partB (input: string list) =
    let time = parseSpacedNumberFromLine input[0]
    let distance = parseSpacedNumberFromLine input[1]
    let (minHold, maxHold) = minMaxTrials (time, distance)
    (maxHold - minHold) + 1L

partB example

partB lines
