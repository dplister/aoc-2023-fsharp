open System
open System.Text.RegularExpressions

let verticalExample = [
    "#.##..##."
    "..#.##.#."
    "##......#"
    "##......#"
    "..#.##.#."
    "..##..##."
    "#.#.##.#."
]

let horizontalExample = [
    "#...##..#"
    "#....#..#"
    "..##..###"
    "#####.##."
    "#####.##."
    "..##..###"
    "#....#..#"
]

let parseLand (input: string list) =
    // array of rows
    let mutable rows = Array.create input.Length []
    // array of columns
    let mutable columns = Array.create input.Head.Length []
    input |> List.iteri (fun y line ->
        line |> Seq.iteri (fun x c ->
            if c = '#' then
                rows[y] <- x :: rows[y]
                columns[x] <- y :: columns[x]
        )
    )
    (rows, columns)

let mirrorLocation (nums: int list array) =
    let rec expand (left: int) (right: int) =
        if left < 0 || right >= nums.Length then
            true
        else if nums[left] <> nums[right] then
            false
        else
            expand (left - 1) (right + 1)
    let rec loop (n: int) =
        if n = nums.Length then
            None
        else
            if expand (n - 1) n then
                Some n
            else
                loop (n + 1)
    loop 1

parseLand verticalExample
|> fun (rows, columns) -> 
    mirrorLocation rows, mirrorLocation columns

parseLand horizontalExample
|> fun (rows, columns) ->
    mirrorLocation rows, mirrorLocation columns

let emptyExpression = Regex "^\s*$"

let splitText (input: string list) =
    let addToSecs (acc: string list) (secs: string list list) =
        if acc.IsEmpty then
            secs 
        else
            acc :: secs
    let rec loop (ls: string list) (acc: string list) (secs: string list list) =
        match ls with
        | head :: tail when (emptyExpression.IsMatch head) ->
            loop tail [] (addToSecs (List.rev acc) secs)
        | head :: tail ->
            loop tail (head :: acc) secs
        | [] ->
            (addToSecs (List.rev acc) secs)
    List.rev (loop input [] [])

let partA (input: string list) =
    splitText input
    |> List.map (fun ls -> 
        parseLand ls
        |> fun (rows, columns) ->
            let rowLoc = mirrorLocation rows
            if rowLoc.IsSome then 
                rowLoc.Value * 100
            else 
                let colLoc = mirrorLocation columns
                colLoc.Value
    )
    |> List.sum

let fullExample = 
    List.append
        (List.append verticalExample [""])
        horizontalExample
partA fullExample

let lines = (IO.File.ReadAllLines "day13inp.txt") |> List.ofSeq
partA lines