open System.IO

let readFile () = 
    File.ReadLines "input.txt"
    |> Seq.toList

let parse (l: string)  =
    l.Split(" ")
    |> Array.map int64
    |> Array.toList

let rec findDiffs acc curr =
    let zeroes = List.filter (fun n -> n = 0L) curr
    if List.length zeroes = List.length curr then curr :: acc
    else 
        let diffs = 
            curr
            |> List.windowed 2
            |> List.map (fun l -> l[1] - l[0])
        findDiffs (curr :: acc) diffs

let calculateHistory (diffs: int64  list list) = 
    diffs
    |> List.tail
    |> List.fold (fun a l -> 
        let last = List.last l
        last + a
    ) 0L

let calculateHistoryBackwards (diffs: int64  list list) = 
    diffs
    |> List.tail
    |> List.fold (fun a l -> 
        let first = List.head l
        first - a
    ) 0L

let diffLists = 
    readFile ()
    |> List.map parse
    |> List.map (findDiffs [])

let task1 =
    diffLists
    |> List.map calculateHistory
    |> List.sum
let task2 =
    diffLists
    |> List.map calculateHistoryBackwards
    |> List.sum

printfn $"Task 1: {task1}" // 1934898178
printfn $"Task 2: {task2}" // 1129
