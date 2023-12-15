open System.IO

let readFile () = 
    File.ReadLines "input.txt"
    |> Seq.toList

type Operation = Remove of string | Add of string * int64

let getLabel (o: Operation) = 
    match o with
    | Remove l -> l
    | Add (l, _) -> l

let getValue (o: Operation) = 
    match o with
    | Remove l -> 0L
    | Add (_, n) -> n

let hash (s: string) = 
    s
    |> Seq.fold (fun acc c -> 
        ((acc + (int64 c)) * 17L) % 256L
    ) 0L

let parseOperation (s: string) = 
    if s.Contains("=") then
        let parts = s.Split("=")
        Add (parts[0], int64 parts[1])
    else if s.Contains("-") then
        let parts = s.Split("-")
        Remove (parts[0])
    else 
        failwith "invalid operation"

let instructions = 
    readFile()
    |> List.head
    |> fun s -> s.Split(",")

let task1 =
    instructions
    |> Array.map hash
    |> Array.sum
let task2 = 
    let boxes = 
        instructions
        |> Array.map parseOperation
        |> Array.fold (fun (acc: Map<int, Operation list>) op -> 
            let label = getLabel op
            let boxId = hash label |> int
            let boxContent = 
                match Map.tryFind boxId acc with
                | Some content -> content
                | None -> []

            match op with
            | Remove l -> 
                let idx = List.tryFindIndex (fun op -> getLabel op = label) boxContent
                match idx with
                | Some i -> Map.add boxId (List.removeAt i boxContent) acc
                | None -> acc
            | Add (l, n) -> 
                let idx = List.tryFindIndex (fun op -> getLabel op = label) boxContent
                match idx with
                | Some i -> 
                    let newContent = 
                        boxContent 
                        |> List.removeAt i 
                        |> List.insertAt i op
                    Map.add boxId newContent acc
                | None -> 
                    Map.add boxId (op :: boxContent) acc
        ) Map.empty
    [0 .. 256]
    |> List.map (fun i -> 
        let box = Map.tryFind i boxes
        match box with 
        | Some content -> 
            match content with
            | [] -> 0L
            | l -> 
                l 
                |> List.rev 
                |> List.mapi (fun j op -> (int64 (i + 1)) * (int64 (j + 1)) * (getValue op) ) 
                |> List.sum
        | None -> 0L
        )
    |> List.sum


printfn $"Task 1: {task1}" // 519041
printfn $"Task 2: {task2}" // 260530
