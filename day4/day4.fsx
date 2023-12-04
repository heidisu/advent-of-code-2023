
open System.IO

let readFile () = 
    File.ReadLines "input.txt"
    |> Seq.toList

let task1 = "task 1"
let task2 = "task 2"

printfn $"Task 1: {task1}"
printfn $"Task 2: {task2}"
