open System.IO
open System

let readFile () = 
    File.ReadLines "input.txt"
    |> Seq.toList

let parse (s: string) = 
    let parts = s.Split(" ")
    let springs = parts[0]|> Seq.toList
    let numbers = 
        parts[1].Split(",")
        |> Seq.toList
        |> List.map (fun c -> c |> string |> int)
    springs, numbers

let isValid (s: string) (numbers: int list) = 
    let parts = s.Split(".") |> Array.filter (fun c -> c <> "")
    if Array.length parts <> List.length numbers then false
    else 
        parts 
        |> Array.map (fun s -> s.Length)
        |> Array.toList
        |> List.zip numbers
        |> List.forall (fun (x, y) -> x = y)

let rec possiblePaths (acc: char list list) (l: char list) =
    match l with
    | [] -> acc |> List.map List.rev |> List.map String.Concat
    | x :: xs -> 
        match x with
        | '?' -> 
            possiblePaths acc ('.' :: xs) @ possiblePaths acc ('#' :: xs)
        | '.' 
        | '#' -> 
            let newAcc = acc |> List.collect (fun l -> [x] |> List.map (fun c -> c :: l))
            possiblePaths newAcc xs
        | c -> failwithf "illegal char %c" c

let validPaths = 
    readFile ()
    |> List.map parse
    |> List.map (fun (springs, numbers) -> 
    let paths = possiblePaths [[]] springs
    paths 
    |> List.filter (fun s -> isValid s numbers)
    )

// https://github.com/Crazytieguy/advent-of-code/blob/c75e0008119a1409fc4e99c302da38804ea96bf8/2023/src/bin/day12/main.rs
let rec findPossiblePaths (springs: char list) (numbers: int list) (cache: int64[,]) =
    match numbers with 
    | [] -> if List.contains '#' springs then 0L else 1L
    | _ -> 
        let springsLength = List.length springs
        let numbersLength  = List.length numbers
        if springsLength < List.sum numbers + (numbersLength - 1) 
        then 0L
        else 
            match cache[springsLength - 1, numbersLength - 1] with
            | -1L ->
                let pathsNextOperational = 
                    if List.head springs <> '#' then 
                        findPossiblePaths (List.tail springs) numbers cache
                    else 0L
                let groupSize = List.head numbers
                let spring = List.take groupSize springs
                let nextAfter = 
                    List.tryItem groupSize springs |> Option.defaultValue '+'
                let pathNextDamaged =
                    if not <| List.contains '.' spring && nextAfter <> '#' then
                        findPossiblePaths (List.skip (groupSize + 1) springs) (List.tail numbers) cache
                    else 0L   
                let possiblePaths = pathsNextOperational + pathNextDamaged
                cache[springsLength - 1,numbersLength - 1] <- possiblePaths
                possiblePaths
            | n -> n

let task1 =
    validPaths
    |> List.map (List.length >> int64)
    |> List.sum
let task2 =
    readFile ()
    |> List.map parse
    |> List.map (fun (s, n) -> 
        let st = String.Concat s
        let bigSprings = 
            String.Join('?', [st; st; st; st; st]) + "."
            |> Seq.toList
        let bigNumbers = 
            List.replicate 5 n
            |> List.collect id
        let cache = Array2D.create (List.length bigSprings) (List.length bigNumbers) -1L
        findPossiblePaths bigSprings bigNumbers cache
        )
    |> List.sum

printfn $"Task 1: {task1}" // 7716
printfn $"Task 2: {task2}" // 18716325559999
