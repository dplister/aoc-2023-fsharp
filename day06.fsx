open System
open System.Text.RegularExpressions

let numberExpression = Regex "([0-9]+)"

let parseNumbersFromLine input =
    numberExpression.Matches input 
    |> Seq.map (fun i -> i.Value |> int)

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
let travel (holdTime: int) (time: int) (distance: int) =
    (time - holdTime) * holdTime

0 = travel 0 7 9
6 = travel 1 7 9
10 = travel 2 7 9
12 = travel 3 7 9

let minMaxTrials ((time, distance): int * int) =
    let findTravel = Seq.find (fun h -> (travel h time distance) > distance)
    // find the minimum to succeed
    let minHold = 
        seq { 1..time }
        |> findTravel
    // find the maximum to succeed
    let maxHold =
        seq { time..(-1)..1 }
        |> findTravel
    (minHold, maxHold)

(4, 11) = minMaxTrials (15, 40)
(11, 19) = minMaxTrials (30, 200)
    
let partA (input: string list) =
    parseTrials input
    |> Seq.map minMaxTrials
    |> Seq.map (fun (minHold, maxHold) -> (maxHold - minHold) + 1)
    |> Seq.reduce (*)

partA example

let lines = (IO.File.ReadAllLines "day06inp.txt") |> List.ofSeq
partA lines