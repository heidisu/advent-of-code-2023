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

let hand = { Cards =  [ A ]; Type = FiveOfAKind; Bid = 0L}

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

let cardToValue2 = function
    | A -> 14
    | K -> 13
    | Q -> 12
    | T -> 11
    | Nine -> 10
    | Eight -> 9 
    | Seven -> 8
    | Six -> 7
    | Five -> 6
    | Four -> 5
    | Three -> 4
    | Two -> 3
    | J -> 2

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

let getType2 (cards: Card list) =
    let groups = cards |> List.groupBy id
    let numOtherGroups = List.length groups - 1
    match List.tryFind (fun (c, _) -> c = J) groups with
    | Some (_, l) ->
        let size = List.length l
        if size = 4 || size = 5 then FiveOfAKind
        else if size = 3 then
            if numOtherGroups = 1 then FiveOfAKind
            else FourOfAKind
        else if size = 2 then
            if numOtherGroups = 3 then ThreeOfAKind
            else if numOtherGroups = 2 then FourOfAKind
            else if numOtherGroups = 1 then FiveOfAKind
            else FiveOfAKind
        else if size = 1 then
            let indexJ = List.findIndex (fun c -> c = J) cards
            [A; K; Q; T; Nine; Eight; Seven; Six; Five; Four; Three; Two]
            |> List.map (fun c ->
                    cards
                    |> List.insertAt indexJ c
                    |> List.removeAt (indexJ + 1))
            |> List.map (fun cards ->
                    let cardType = getType cards
                    (cardType, typeToValue cardType))
            |> List.sortByDescending snd
            |> List.head
            |> fst
        else failwith "error"
    | None -> getType cards
    
let readFile () = 
    File.ReadLines "input.txt"
    |> Seq.toList

let parse (getType: Card list -> CardType) (l: string) =
    let parts = l.Split(" ")
    let bid = parts[1] |> int64
    let cards = parts[0] |> Seq.map charToCard |> Seq.toList
    {
        Cards = cards
        Bid = bid
        Type = getType cards
    }

let comparer (cardToValue: Card -> int) (first: Hand) (second: Hand) =
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
    |> List.map (parse getType)
    |> List.sortWith (comparer cardToValue)
    |> List.mapi (fun i h -> (int64 (i + 1)) * h.Bid)
    |> List.sum
let task2 =
    readFile ()
    |> List.map (parse getType2)
    |> List.sortWith (comparer cardToValue2)
    |> List.mapi (fun i h -> (int64 (i + 1)) * h.Bid)
    |> List.sum

printfn $"Task 1: {task1}" // 253954294
printfn $"Task 2: {task2}" // 254837398
