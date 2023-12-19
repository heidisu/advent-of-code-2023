open System.IO
open System

let readFile () = 
    File.ReadLines "input.txt"
    |> Seq.toList

type Direction = Up | Down | Left | Right

let toDir (s: string) = 
    match s with
    | "U" -> Up
    | "D" -> Down
    | "R" -> Right
    | "L" -> Left
    | _ -> failwith "illegal direction"

let parse (s: string) = 
    let parts = s.Split(" ")
    (toDir (parts[0]), parts[1] |> int)

let parseHex (s: String) =
    let hex = (s.Split("(#")[1]).Split(")")  |> Array.head
    let digit = hex.Substring(0, 5)
    let direction = hex.Substring(5) |> int
    let dir =
        match direction with
        | 0 -> Right
        | 1 -> Down
        | 2 -> Left
        | 3 -> Up
        | _ -> failwith "illegal direction"
    (dir, Convert.ToInt64(digit, 16))
        
let rec dig acc (x, y) digplan = 
    match digplan with
    | [] -> acc
    | (dir, steps) :: xs -> 
        let (d1, d2) = 
            match dir with
            | Up -> (-1, 0)
            | Down -> (1, 0)
            | Left -> (0, -1)
            | Right -> (0, 1)
        let points = 
            [1 .. steps]
            |> List.map(fun i -> (x + i * d1, y + i * d2))
        dig (points @ acc) (List.last points) xs

let rec mergeSet counter firstSet otherSet prev =
    if prev = firstSet then prev
    else
        if counter % 100 = 0 then
            printfn "counter %d firstSet %A" counter (Set.count firstSet)
        let neighbours =
            firstSet
            |>Set.toList
            |> List.collect (fun (x, y) -> [(x - 1, y); (x + 1, y); (x, y - 1); (x, y + 1)])
            |> Set.ofList
        let intersection = Set.intersect neighbours otherSet
        let newFirst = Set.union firstSet intersection
        let newOther = Set.difference otherSet intersection
        mergeSet (counter + 1) newFirst newOther firstSet

let digplan = 
    readFile ()
    |> List.map parse

let task1 =
    let dug = 
        digplan
        |> dig [] (0, 0)
    let minHeight = dug |> List.minBy fst |> fst
    let minWidth = dug |> List.minBy snd |> snd
    let transX = if minHeight < 0 then -minHeight + 1 else 1
    let transY = if minWidth < 0 then -minWidth + 1 else 1
    let translated = 
        dug
        |> List.map (fun (x, y) -> (x + transX, y + transY))
        |> List.sort
    let height = translated |> List.maxBy fst |> fst |> (+) 1
    let width = translated |> List.maxBy snd |> snd |> (+) 1
    let notVisited = 
        [0 .. height]
        |> List.collect (fun i -> 
            [0 .. width]
            |> List.map (fun j -> (i, j))
        )
        |> List.filter (fun (i, j) -> not <| List.contains (i, j) translated)
        |> Set.ofList
    
    let startSet = Set.ofList [(0,0)]
    let otherSet = Set.remove (0, 0) notVisited
    let outside = mergeSet 0 startSet otherSet Set.empty
    let total = (height + 1) * (width + 1)
    total - (Set.count outside)

let task2 =
    let distances =
        readFile ()
        |> List.map parseHex
    let points = 
        distances
        |> List.fold (fun (acc, prev) (dir, length) ->
            let (x, y) = prev
            let next =
                match dir with
                | Up -> (x - length, y)
                | Down -> (x + length, y)
                | Left -> (x, y - length)
                | Right -> (x, y + length)
            (prev :: acc, next)
            ) ([], (0L,0L))
        |> fst
    // shoelace formula https://en.wikipedia.org/wiki/Shoelace_formula
    let area =
        points
        |> List.pairwise
        |> List.map (fun ((x1, y1), (x2, y2)) -> (x1 * y2) - (x2 * y1))
        |> List.sum
        |> (fun n -> n / 2L)
    let edgePoints =
        distances |> List.map snd |> List.sum
    // Pick's theorem, solved for i https://en.wikipedia.org/wiki/Pick%27s_theorem      
    let interiorPoints = area - (edgePoints / 2L) + 1L
    interiorPoints + edgePoints

printfn $"Task 1: {task1}" // 49578
printfn $"Task 2: {task2}" // 52885384955882
