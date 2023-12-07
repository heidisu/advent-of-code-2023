open System.IO

let readFile () = 
    File.ReadLines "test-input.txt"
    |> Seq.toList

type Color = Red | Blue | Green
type Round = (Color * int) list 
type Game = int * Round list

let stringToColor str = 
    match str with
    | "blue" -> Blue
    | "red" -> Red
    | "green" -> Green
    | _ -> failwith "invalid color"

let parse (s: string) : Game = 
    let g = s.Split(":")
    let id = g[0].Split(" ")[1]
    let rounds = 
        g[1].Split(";")
        |> Array.map (fun l ->
            let colors = 
                l.Split(",")
                |> Array.map (fun c ->
                    let parts = c.Split(" ")
                    let color = stringToColor parts[2]
                    let number = int parts[1]
                    (color, number)
                ) |> Array.toList
            colors
        )|> Array.toList
    (int id, rounds)
    
let limit = 
    [
        (Red, 12)
        (Green, 13)
        (Blue, 14)
    ]

let isWithinLimit cnt color = 
    limit 
    |> List.find (fun (c, i) -> c = color) 
    |> snd
    |> (fun i -> i >= cnt)       

let isValid (game: Game): bool =
    snd game
    |> List.fold (fun s r ->
        if s
        then 
            r|> List.fold (
                fun s (c, i) -> 
                    if s
                    then isWithinLimit i c
                    else s
                    ) true
        else s
    ) true

let maxColor (rounds: Round list) (color: Color): int =
    rounds
    |> List.choose (
        fun r -> r |> List.tryFind (fun (c, i) -> c = color)
    ) |> List.maxBy snd
    |> snd

let minBlocks ((i, rnds): Game) =
    let red = maxColor rnds Red
    let blue = maxColor rnds Blue
    let green = maxColor rnds Green
    (int64 red) * (int64 blue) * (int64 green)


let task1 = 
    readFile ()
    |> List.map parse
    |> List.filter isValid 
    |> List.map fst
    |> List.sum 
let task2 =
    readFile ()
    |> List.map parse
    |> List.map minBlocks
    |> List.sum

printfn $"Task 1: {task1}" // 2268
printfn $"Task 2: {task2}" // 63542
