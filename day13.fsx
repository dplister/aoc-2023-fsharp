open System
open System.Text.RegularExpressions
open System.Linq

// --- Part A ---

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

// --- Part B ---

let listsAlmostMatch (l1: int list) (l2: int list) =
    if abs(l1.Length - l2.Length) > 1 then 
        None
    else 
        let rec loop ls1 ls2 ctr = 
            if ctr > 1 then 
                ctr
            else 
                match ls1, ls2 with
                | [], [] -> ctr
                | [], lsl2 -> 
                    lsl2.Length + ctr
                | lsl1, [] -> 
                    lsl1.Length + ctr
                | l1head :: l1tail, l2head :: l2tail 
                    when l1head = l2head ->
                    loop l1tail l2tail ctr
                | l1head :: l1tail, l2head :: l2tail 
                    when l1head <> l2head ->
                    // if the lists are the same length it means l1 has an item l2 doesn't have and vice versa
                    // i.e. there are two differences
                    if l1tail.Length = l2tail.Length then
                        99
                    else
                        loop 
                            (if l2tail.Length > l1tail.Length then ls1 else l1tail)
                            (if l1tail.Length > l2tail.Length then ls2 else l2tail)
                            (ctr + 1)
        let res = loop l1 l2 0
        if res > 1 then None else Some res

Some 0 = listsAlmostMatch [1;2;3] [1;2;3]
Some 1 = listsAlmostMatch [1;2;3] [1;2;3;4]
Some 1 = listsAlmostMatch [1;3] [1;2;3]
Some 1 = listsAlmostMatch [1;2;3] [2;3]
Some 1 = listsAlmostMatch [1;2;4] [1;2;3]
None = listsAlmostMatch [1] [1;2;3]

let mirrorWithMissing (nums: int list array) =
    let rec expand (left: int) (right: int) (missing: int) =
        if missing > 1 then 
            false
        else if left < 0 || right >= nums.Length then
            missing = 1
        else
            match listsAlmostMatch nums[left] nums[right] with
            | Some v -> if v + missing > 1 then false else expand (left - 1) (right + 1) (v + missing)
            | None -> false
    let rec loop (n: int) =
        if n = nums.Length then
            None
        else
            if expand (n - 1) n 0 then
                Some n
            else
                loop (n + 1)
    loop 1

// note: vertical example due to smudge now has horizontal split at line 3/4
parseLand verticalExample
|> fun (rows, columns) -> 
    mirrorWithMissing rows, mirrorWithMissing columns

// note: horizontal has now shifted to line 1/2
parseLand horizontalExample
|> fun (rows, columns) ->
    mirrorWithMissing rows, mirrorWithMissing columns

let partB (input: string list) =
    splitText input
    |> List.map (fun ls -> 
        parseLand ls
        |> fun (rows, columns) ->
            let rowLoc = mirrorWithMissing rows
            if rowLoc.IsSome then 
                rowLoc.Value * 100
            else 
                let colLoc = mirrorWithMissing columns
                colLoc.Value
    )
    |> List.sum

partB fullExample

partB lines

// 34020 too small