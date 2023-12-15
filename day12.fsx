open System
open System.Text.RegularExpressions

let numberExpression = Regex "([\-0-9]+)"

type Spring = {
    Indexes: int list
    Path: char array
    Nums: int list
}

let findIndexes (input: char array) =
    let mutable inds = []
    for x in [0..(input.Length - 1)] do
        if (input[x] = '#' || input[x] = '?')
            && (x = 0 || input[x - 1] = '.') then
            inds <- x :: inds
    List.rev inds

let parseLine (input: string) =
    let tokens = input.Split ' '
    let path = tokens[0] |> Array.ofSeq
    let indexes = findIndexes path
    {
        Path = path
        Nums = numberExpression.Matches tokens[1]
            |> Seq.map (fun m -> m.Value |> int)
            |> List.ofSeq
        Indexes = indexes
    }

let example = [
    "???.### 1,1,3"
    ".??..??...?##. 1,1,3"
    "?#?#?#?#?#?#?#? 1,3,1,6"
    "????.#...#... 4,1,1"
    "????.######..#####. 1,6,5"
    "?###???????? 3,2,1"
]

parseLine example[0]