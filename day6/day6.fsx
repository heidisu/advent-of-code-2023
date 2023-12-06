open System.IO
open System

let readFile () = 
    File.ReadLines "input.txt"
    |> Seq.toList

let parseMultiNumberLine (l: string) = 
    (l.Split(":")[1]).Trim().Split(" ")
        |> Array.filter (fun s -> s <>"")
        |> Array.map int64
        |> Array.toList

let parseSingleNumberLine (l: string) = 
    (l.Split(":")[1]).Trim().Split(" ")
    |> Array.fold (fun a s -> 
        if s <> "" then a + s
        else a) ""
    |> String.Concat
    |> int64

let parse (l1: string) (l2: string) = 
    let times = parseMultiNumberLine l1
    let records = parseMultiNumberLine l2
    List.zip times records

let parse2 (l1: string) (l2: string): int64 * int64 = 
    let times = parseSingleNumberLine l1
    let records = parseSingleNumberLine l2
    (times, records)

let calculateRecords (time: int64) (record: int64) = 
    [1 .. (int time)]
    |> List.map int64
    |> List.map (fun charge ->
        let remaining = time - charge
        remaining * charge)
    |> List.filter (fun i -> i > record)
    |> List.length

let task1 = 
    let lines = readFile ()
    parse lines[0] lines[1]
    |> List.map (fun (t, r) -> calculateRecords t r)
    |> List.fold ( * ) 1
let task2 = 
    let lines = readFile ()
    let (time, record) = parse2 lines[0] lines[1]
    calculateRecords time record

printfn $"Task 1: {task1}" //449550
printfn $"Task 2: {task2}" // 28360140
