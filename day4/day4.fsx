open System.IO

let readFile () = 
    File.ReadLines "test-input.txt"
    |> Seq.toList

let parseNumbers (s: string) = 
    s.Split(" ")
    |> Array.filter (fun s -> s <> "")
    |> Array.map (fun (s: string) -> int s)
    |> Array.toList

let parse (s: string) =
    let parts = (s.Split(": ")[1]).Split(" | ")
    let winners = parseNumbers parts[0]
    let cardNumbers = parseNumbers parts[1]
    (winners, cardNumbers)

let calculate (winners, cardNumbers) = 
    cardNumbers
    |> List.fold (fun acc i -> 
            if List.contains i winners 
            then if acc = 0 then 1 else acc * 2
            else acc    
        ) 0

let numberOfMathces (winners, cardNumbers) = 
    cardNumbers
    |> List.filter (fun c -> List.contains c winners)
    |> List.length

let rec calculate2 cards idx (acc: int) =
    let numCards = List.item idx cards
    if numCards = 0 then acc + 1
    else 
        [1 .. numCards]
        |> List.map (fun i -> calculate2 cards (i + idx) acc)
        |> List.sum
        |> (+) 1

let task1 =
    readFile ()
    |> List.map parse
    |> List.map calculate
    |> List.sum
let task2 =
    let cardMatches = 
        readFile ()
        |> List.map parse
        |> List.map numberOfMathces
    cardMatches
    |> List.mapi (fun i n -> calculate2 cardMatches i 0)
    |> List.sum

printfn $"Task 1: {task1}" // 20667
printfn $"Task 2: {task2}" // 5833065
