open System
open System.Text.RegularExpressions

// --- Part A ---

let labels = ['A';'K';'Q';'J';'T';'9';'8';'7';'6';'5';'4';'3';'2']
let labelScores =
    List.zip 
        labels
        [labels.Length..(-1)..1]
    |> Map.ofList

type HandScore = FiveOfAKind=7 | FourOfAKind=6 | FullHouse=5 | ThreeOfAKind=4 | TwoPair=3 | OnePair=2 | HighCard=1 | Unscored=0

type Hand = {
    Cards: char list
    Bet: int
    HandStrength : HandScore
    CardValues : int list
}

let scoreGroup (groupedHand: List<char * char list>) =
    if groupedHand.Length = 1 then // five cards same label
        HandScore.FiveOfAKind
    else if groupedHand.Length = 2 && (snd groupedHand[0]).Length = 4 then // four cards same label
        HandScore.FourOfAKind
    else if groupedHand.Length = 2 then // three cards same, two cards same
        HandScore.FullHouse
    else if groupedHand.Length = 3 && (snd groupedHand[0]).Length = 3 then // three cards same, two cards different
        HandScore.ThreeOfAKind
    else if (snd groupedHand[0]).Length = 2 && (snd groupedHand[1]).Length = 2 then // two cards same, two cards same
        HandScore.TwoPair
    else if (snd groupedHand[0]).Length = 2 then // two cards same
        HandScore.OnePair
    else
        HandScore.HighCard

let scoreHand (hand: char list) =
    hand
        |> List.groupBy (fun c -> c)
        |> List.sortByDescending (fun (_, ls) -> ls.Length)
        |> scoreGroup

let handExpression = Regex "([0-9AKQJT]{5}) ([0-9]+)"

let parseHand (line: string) =
    let res = handExpression.Match line
    {
        Cards = res.Groups[1].Value |> Seq.toList
        Bet = res.Groups[2].Value |> int
        HandStrength = HandScore.Unscored
        CardValues = []
    }

{ Cards = ['3';'2';'T';'3';'K']; Bet = 765; HandStrength = HandScore.Unscored; CardValues = []} = parseHand "32T3K 765"

// identifies what kind of hand and scores the individual cards
let rankHand (labelScores: Map<char, int>) (hand: Hand) =
    { 
        hand with 
            HandStrength = scoreHand hand.Cards 
            CardValues = hand.Cards |> List.map (fun c -> labelScores[c])
    }

let sortHands (hands: Hand list) =
    hands 
    |> List.sortBy (fun h -> h.HandStrength, h.CardValues)

let example = [
    "32T3K 765"
    "T55J5 684"
    "KK677 28"
    "KTJJT 220"
    "QQQJA 483"
]

let partA (lines: string list) =
    lines
    |> List.map (fun l -> l |> parseHand |> rankHand labelScores)
    |> List.sortBy (fun h -> h.HandStrength, h.CardValues)
    |> List.zip [1..lines.Length]
    |> List.map (fun (rank: int, hand: Hand) -> rank * hand.Bet)
    |> List.sum

partA example

let lines = (IO.File.ReadAllLines "day07inp.txt") |> List.ofSeq
partA lines

// --- Part B ---

let revisedLabels = ['A';'K';'Q';'T';'9';'8';'7';'6';'5';'4';'3';'2';'J']
let revisedLabelScores =
    List.zip 
        revisedLabels
        [revisedLabels.Length..(-1)..1]
    |> Map.ofList

let mergeJokers (groups: List<char * char list>) =
    if groups.Length = 1 then 
        groups
    else 
        let splitter = groups |> List.tryFind (fun g -> (fst g) = 'J')
        match splitter with
        | Some (_, js) -> 
            let jokerlessGroup = groups |> List.filter (fun g -> not((fst g) = 'J'))
            let (c, ls) = jokerlessGroup[0]
            (c, List.append ls js) :: jokerlessGroup.Tail
        | None -> groups

mergeJokers [('A', ['A';'A']); ('J', ['J'])]

let scoreHandRevised (hand: char list) =
    hand
        |> List.groupBy (fun c -> c)
        |> List.sortByDescending (fun (_, ls) -> ls.Length)
        |> mergeJokers
        |> scoreGroup

// identifies what kind of hand and scores the individual cards
let rankHandRevised (scores: Map<char, int>) (hand: Hand) =
    { 
        hand with 
            HandStrength = scoreHandRevised hand.Cards 
            CardValues = hand.Cards |> List.map (fun c -> scores[c])
    }

let partB (lines: string list) =
    lines
    |> List.map (fun l -> l |> parseHand |> rankHandRevised revisedLabelScores)
    |> List.sortBy (fun h -> h.HandStrength, h.CardValues)
    |> List.zip [1..lines.Length]
    |> List.map (fun (rank: int, hand: Hand) -> rank * hand.Bet)
    |> List.sum

partB example

partB lines

// 426350789 is wrong