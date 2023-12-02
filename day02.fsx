open System;
open System.Text.RegularExpressions

let grabExpression = Regex "([0-9]+) ([a-z]+)"
let gameExpression = Regex "Game ([0-9]+): (.+)"

type Grab = {
    Colour: string
    Total: int
}

let splitRound (input: string) = 
    input.Split ';'

let parseGrabValues (input: string) =
    grabExpression.Matches input
    |> Seq.map (fun m -> 
        let total = m.Groups[1].Value |> int
        let colour = m.Groups[2].Value
        { Total = total; Colour = colour }
    )

let parseRounds (input: string) =
    splitRound input
    |> Seq.map parseGrabValues

let parseGame (input: string) = 
    let game = gameExpression.Match input
    let gameNum = game.Groups[1].Value |> int
    let rounds = game.Groups[2].Value |> parseRounds 
    {| Num = gameNum; Rounds = rounds |}

let colours = [
    "red", 12;
    "green", 13;
    "blue", 14] 

/// determines if the set of grabs matches the colour maximums
let roundConforms (maximums: Map<string,int>) (grabs: seq<Grab>) =
    grabs
    |> Seq.forall (fun g -> 
        let col = maximums[g.Colour]
        g.Total <= col
    )

let partA (input: seq<string>) = 
    let maximums = colours |> Map.ofList
    let rules = (roundConforms maximums)
    input 
    |> Seq.map parseGame // extract game
    |> Seq.filter (fun g -> 
        g.Rounds |> Seq.forall rules
    ) // ensure game does not exceed the cube limits
    |> Seq.map (fun g -> g.Num) // extract game number
    |> Seq.sum

let partAExample = [
    "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green";
    "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue";
    "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red";
    "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red";
    "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
]

8 = partA partAExample

let lines = (IO.File.ReadAllLines "day02inp.txt")
partA lines