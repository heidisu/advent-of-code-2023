open System.IO
open System

let readFile () = 
    File.ReadLines "test-input.txt"
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
    let timeF = float time
    let recordF = float record
    let root = Math.Sqrt(timeF * timeF - 4.0 * recordF)
    let min = (timeF - root) / 2.0
    let max = (timeF + root) / 2.0
    let roundMin = Math.Ceiling min
    let roundMax = Math.Floor max
    
    // correct if roots are integer
    let corrMin = if min = roundMin then min + 1.0  else roundMin
    let corrMax = if max = roundMax then max - 1.0 else roundMax
    corrMax - corrMin + 1.0 |> int64

let task1 = 
    let lines = readFile ()
    parse lines[0] lines[1]
    |> List.map (fun (t, r) -> calculateRecords t r)
    |> List.fold ( * ) 1L
let task2 = 
    let lines = readFile ()
    let (time, record) = parse2 lines[0] lines[1]
    calculateRecords time record

printfn $"Task 1: {task1}" //449550
printfn $"Task 2: {task2}" // 28360140
