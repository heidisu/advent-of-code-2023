open System.IO
open System

let readFile () = 
    File.ReadLines "input.txt"
    |> Seq.toList

let parsemultiNumberLine (l: string) = 
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
    let times = parsemultiNumberLine l1
    let records = parsemultiNumberLine l2
    List.zip times records

let parse2 (l1: string) (l2: string): int64 * int64 = 
    let times = parseSingleNumberLine l1
    let records = parseSingleNumberLine l2
    (times, records)

let calculcateRecord (time: int64) (record: int64) = 
    [1 .. (int time)] 
    |> List.map (fun charge ->
        let c = int64 charge
        let remaining = time - c
        remaining * c)
    |> List.filter (fun i -> i > record)
    |> List.length

let task1 = 
    let lines = readFile ()
    parse lines[0] lines[1]
    |> List.map (fun (t, r) -> calculcateRecord t r)
    |> List.fold ( * ) 1
let task2 = 
    let lines = readFile ()
    let (time, record) = parse2 lines[0] lines[1]
    calculcateRecord time record

printfn $"Task 1: {task1}" //449550
printfn $"Task 2: {task2}" // 28360140
