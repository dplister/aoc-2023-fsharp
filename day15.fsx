open System
open System.Text.RegularExpressions

// --- Part A ---


let parseLine (input: string) =
    input.Split ',' |> List.ofArray

let example = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"

let hashLetters input =
    (0, input) ||> Seq.fold (fun acc (c: char) -> ((acc + (c |> int)) * 17) % 256)

52 = hashLetters "HASH"
hashLetters "HA"
hashLetters "SH"


let hashSequence (instructions: string list) =
    (0, instructions) ||> Seq.fold (fun acc inst -> acc + (hashLetters inst))

hashSequence (example |> parseLine |> List.ofSeq)

hashSequence ["rn=1"]

let partA (input: string) =
    input 
    |> parseLine
    |> hashSequence

partA example

let lines = (IO.File.ReadAllLines "day15inp.txt") |> Seq.head |> (fun s -> s.Trim())
partA lines

// --- Part B ---

type Lens = {
    Label: string
    FocalLength: int
}

let bindExpression = Regex "^([a-z]+)([=\-])([0-9]*)$"

(bindExpression.Match "rn").Groups
|> Seq.skip 1 |> Seq.map (fun g -> g.Value)

let matchBinding input =
    let m = bindExpression.Match input
    m.Groups |> Seq.skip 1 |> Seq.map (fun g -> g.Value) |> List.ofSeq

matchBinding "rn=1"
matchBinding "cm-"


let updateOrInsert (item: Lens) (lenses: Lens list) =
    let rec loop (ls: Lens list) (acc: Lens list) =
        match ls with
        | [] -> List.rev (item :: acc)
        | head :: tail when head.Label = item.Label ->
            List.append (List.rev acc) (item :: tail)
        | head :: tail when head.Label <> item.Label ->
            loop tail (head :: acc)
    loop lenses []

updateOrInsert {Label = "B"; FocalLength = 1} [{ Label = "A"; FocalLength = 0 }; { Label = "B"; FocalLength = 0 }; { Label = "C"; FocalLength = 0 }]
updateOrInsert {Label = "D"; FocalLength = 1} [{ Label = "A"; FocalLength = 0 }; { Label = "B"; FocalLength = 0 }; { Label = "C"; FocalLength = 0 }]

let rec execute (boxes: Map<int, Lens list>) (instructions: string list) =
    match instructions with 
    | [] -> boxes
    | head :: tail ->
        let item = matchBinding head
        let hash = hashLetters item[0]
        match matchBinding head with 
        | [label; "="; value] ->
            let updatedBoxes = boxes.Change(hash,(fun ls -> 
                let item = { Label = item[0]; FocalLength = item[2] |> int }
                match ls with 
                | None -> Some([item])
                | Some v -> Some(updateOrInsert item v)))
            execute updatedBoxes tail
        | [label; "-"; _] ->
            let updatedBoxes = boxes.Change(hash,(fun ls ->
                match ls with 
                | None -> Some([])
                | Some v -> Some(v |> List.filter (fun itm -> itm.Label <> item[0]))))
            execute updatedBoxes tail
        | _ -> failwith $"Unknown instruction {head}"

let scoreLenses (boxes: Map<int, Lens list>) =
    boxes |> Map.keys |> Seq.fold (fun total k -> 
        let scores = boxes[k] |> Seq.mapi (fun i item -> (k + 1) * (i + 1) * item.FocalLength)
        total + (scores |> Seq.sum)) 0

execute Map.empty (example |> parseLine)
|> scoreLenses

let partB (input: string) =
    execute Map.empty (input |> parseLine)
    |> scoreLenses

partB lines