open System.IO

type Card = 
    | A 
    | K 
    | Q 
    | J 
    | T 
    | Nine 
    | Eight 
    | Seven 
    | Six 
    | Five 
    | Four
    | Three
    | Two

type CardType = 
    | FiveOfAKind
    | FourOfAKind
    | FullHouse
    | ThreeOfAKind
    | TwoPair
    | OnePair
    | HighCard

let typeToValue = function 
    | FiveOfAKind -> 7
    | FourOfAKind -> 6
    | FullHouse -> 5
    | ThreeOfAKind -> 4
    | TwoPair -> 3
    | OnePair -> 2
    | HighCard -> 1

type Hand = {
    Cards: Card list
    Type: CardType
    Bid: int64
}

let charToCard = function
    | 'A' -> A
    | 'K' -> K
    | 'Q' -> Q
    | 'J' -> J
    | 'T' -> T
    | '9' -> Nine
    | '8' -> Eight 
    | '7' -> Seven
    | '6' -> Six
    | '5' -> Five
    | '4' -> Four 
    | '3' -> Three
    | '2' -> Two
    | _ -> failwith "invalid char"

let cardToValue = function
    | A -> 14
    | K -> 13
    | Q -> 12
    | J -> 11
    | T -> 10
    | Nine -> 9
    | Eight -> 8 
    | Seven -> 7
    | Six -> 6
    | Five -> 5
    | Four -> 4 
    | Three -> 3
    | Two -> 2

let getType (cards: Card list) = 
    let groups = cards |> List.groupBy id
    let numGroups = List.length groups
    if numGroups = 1 then FiveOfAKind
    else if numGroups = 5 then HighCard
    else if numGroups = 2 then 
        let first = List.item 0 groups |> snd
        let second = List.item 1 groups |> snd
        if List.length first = 4 || List.length second = 4 then FourOfAKind
        else FullHouse
    else if numGroups = 3 then
        let first = List.item 0 groups |> snd
        let second = List.item 1 groups |> snd
        let third = List.item 2 groups |> snd
        if List.length first = 3 || List.length second = 3 || List.length third = 3
        then ThreeOfAKind
        else TwoPair
    else if numGroups = 4 then OnePair
    else failwith "not valid"

let readFile () = 
    File.ReadLines "input.txt"
    |> Seq.toList

let parse (l: string) =
    let parts = l.Split(" ")
    let bid = parts[1] |> int64
    let cards = parts[0] |> Seq.map charToCard |> Seq.toList
    {
        Cards = cards
        Bid = bid
        Type = getType cards
    }

let comparer (first: Hand) (second: Hand) =
    let firstTypeValue = typeToValue first.Type
    let secondTypeValue = typeToValue second.Type
    if firstTypeValue > secondTypeValue then 1
    else if secondTypeValue > firstTypeValue then -1 
    else 
        let zipped = List.zip first.Cards second.Cards
        match List.tryFind (fun (f, s) -> f <> s) zipped with
        | Some (c1, c2) -> if cardToValue c1 > cardToValue c2 then 1 else -1
        | None -> 0
let task1 =
    readFile ()
    |> List.map parse
    |> List.sortWith comparer
    |> List.mapi (fun i h -> (int64 (i + 1)) * h.Bid)
    |> List.sum
let task2 = "task 2"

printfn $"Task 1: {task1}" // 253954294
printfn $"Task 2: {task2}"
