open System
open System.Text.RegularExpressions

type Card = {
    Game: int
    Chosen: int Set
    Winning: int Set
}

let gameExpression = Regex "Card\s* ([0-9]+): ([^|]+) \| (.*)$"
let numberExpression = Regex "([0-9]+)"

let numberList input =
    numberExpression.Matches input
    |> Seq.map (fun m -> m.Value |> int)

let readLine input =
    let game = gameExpression.Match input
    printfn $"Group 1: {game.Groups[1].Value}"
    { 
        Game = game.Groups[1].Value |> int
        Chosen = numberList game.Groups[2].Value |> Set.ofSeq
        Winning = numberList game.Groups[3].Value |> Set.ofSeq
    }

let example = [
    "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
    "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19"
    "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1"
    "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83"
    "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36"
    "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"
]

let score (card: Card) =
    let matches = Set.intersect card.Chosen card.Winning
    match matches.Count with
    | 0 -> 0
    | 1 -> 1
    | _ -> pown 2 (matches.Count - 1)

let scoreCards (input: string seq) =
    input
    |> Seq.map (fun line -> 
        line
        |> readLine
        |> score)
    |> Seq.sum

scoreCards example

let lines = (IO.File.ReadAllLines "day04inp.txt")
scoreCards lines