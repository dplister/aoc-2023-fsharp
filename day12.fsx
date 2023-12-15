open System
open System.Text.RegularExpressions

// --- Part A ---

let numberExpression = Regex "([\-0-9]+)"

/// splits the input into a path and the set of group nums
let parseLine (input: string) =
    let tokens = input.Split ' '
    let path = tokens[0] |> List.ofSeq
    (
        path,
        numberExpression.Matches tokens[1]
            |> Seq.map (fun m -> m.Value |> int)
            |> List.ofSeq
    )

let example = [
    "???.### 1,1,3"
    ".??..??...?##. 1,1,3"
    "?#?#?#?#?#?#?#? 1,3,1,6"
    "????.#...#... 4,1,1"
    "????.######..#####. 1,6,5"
    "?###???????? 3,2,1"
]

parseLine example[0]

let isPossiblyValid (nums: int list) (input: char list) =
    let rec loop (ns: int list) (cs: char list) (count: int) =
        match cs with 
        | head :: tail when head = '#' ->
            loop ns tail (count + 1)
        | head :: tail when head = '.' ->
            if count > 0 then 
                // we keep finding more broken after numbers ran out
                if ns.Length = 0 then
                    false
                else if ns[0] = count then
                    loop ns.Tail tail 0
                // number must not match
                else
                    false
            else
                loop ns tail 0
        // we can't be certain, bail out
        | head :: _ when head = '?' ->
            true
        // ran out of chars; what we have so far worked
        | _ -> true
    loop nums (input |> List.rev) 0

true = isPossiblyValid [1;2;3] ("#.##.???" |> List.ofSeq |> List.rev)
false = isPossiblyValid [1;2;3] ("#..#.???" |> List.ofSeq |> List.rev)
true = isPossiblyValid [1;1;3] ("#..#.???" |> List.ofSeq |> List.rev)
true = isPossiblyValid [1;1;3] ("#..#.###" |> List.ofSeq |> List.rev)

/// generates the set of path permutations that can _possibly_ fit the group list
let generatePathPermutations (nums: int list) (input: char list) =
    let rec loop (lines: char list list) (tokens: char list) =
        match tokens with 
        | head :: tail when head = '.' || head = '#' -> 
            loop (lines |> List.map (fun l -> head :: l)) tail
        | _ :: tail ->
            let merged = 
                List.append 
                    (lines |> List.map (fun l -> '#' :: l))
                    (lines |> List.map (fun l -> '.' :: l))
                |> List.filter (fun l -> isPossiblyValid nums l)
            loop merged tail
        | [] -> lines |> List.map (fun l -> List.rev l |> String.Concat)
    loop [[]] input

generatePathPermutations [1;1;3] ("???.###" |> List.ofSeq)

/// splits the input across the set of groups of '#'
let splitGroups (input: string) = 
    input.Split('.',StringSplitOptions.RemoveEmptyEntries)

splitGroups ".#...#....###."

/// determines if the ordered set of nums comforms to the set of groups found in input
let isValidSolution (nums: int list) (input: string) =
    let rec exact (l1: int list) (l2: int list) =
        match l1, l2 with
        | head1 :: tail1, head2 :: tail2  when head1 = head2 ->
            exact tail1 tail2
        | [], [] -> true
        | _ -> false
    let groups = 
        splitGroups input 
        |> Seq.map (fun (g: string) -> g.Length)
        |> List.ofSeq
    exact nums groups

true = isValidSolution [1;1;3] ".#.#...###"  
false = isValidSolution [3;1;2] "###.#.##..#"
true = isValidSolution [3;2;1] ".###....##.#"

/// finds all solutions conforming to the set of group nums specified
let allSolutions (nums: int list) (input: char list) =
    input
    |> generatePathPermutations nums
    |> List.filter (fun p -> isValidSolution nums p)
    |> List.distinct

example[5]
|> parseLine
|> (fun (l, ns) -> allSolutions ns l)

let partA (input: string list) =
    input
    |> List.map (fun l ->
        let path, nums = parseLine l
        (allSolutions nums path).Length
    )
    |> List.sum

partA example

let lines = (IO.File.ReadAllLines "day12inp.txt") |> List.ofSeq
// partA lines

// --- Part B ---

let unfoldLine (path: char list) (nums: int list) =
    let longerLine = 
        ([0..4], (List.replicate 5 path)) 
            ||> List.map2 (fun i p -> if i = 0 then p else '?' :: p) 
            |> List.concat
    (
        longerLine,
        List.replicate 5 nums |> List.concat
    )

unfoldLine ['.';'#'] [1]
||> (fun p ns -> 
    (p |> String.Concat) = ".#?.#?.#?.#?.#"
    && (ns = [1;1;1;1;1]))

let partB (input: string list) =
    input
    |> List.map (fun l ->
        let path, nums = parseLine l ||> unfoldLine
        (allSolutions nums path).Length
    )
    |> List.sum

// printfn "%A" (partB example)
printfn "%A" (partB lines)