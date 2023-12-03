open System.IO
open System

let readFile () = 
    File.ReadLines "input.txt"
    |> Seq.toList

let neighbours grid  i j =
    let height = Array.length grid
    let width = Array.length grid[0]

    let neighbours = [
        (i - 1, j - 1)
        (i - 1, j)
        (i - 1, j + 1 )
        (i, j - 1)
        (i, j + 1)
        (i + 1, j - 1)
        (i + 1, j)
        (i + 1, j + 1)
    ]
    neighbours
    |> List.filter (fun (x, y) -> x >= 0 && x < width && y >= 0 && y < height)

let rec searchForward (row: char array) j width acc idx =
    if j >= width then (acc, idx)
    else if Char.IsDigit row[j] then searchForward row (j + 1)  width (acc @ [row[j]]) (j :: idx)
    else (acc, idx)

let rec searchBackward (row: char array) j acc idx =
    if j < 0 then (acc, idx)
    else if Char.IsDigit row[j] then searchBackward row (j - 1) (row[j] :: acc) (j :: idx)
    else (acc, idx)


let findNumber (grid: char array array) i j = 
    let (back, backIdx) = searchForward grid[i] (j + 1) (Array.length (grid[i])) [] []
    let (front, frontIdx) = searchBackward grid[i]  (j - 1) [] []
    let total = front @ [grid[i][j]] @ back
    let idxset = j :: backIdx @ frontIdx |> Set.ofList
    (total |> String.Concat |> int, i, idxset )

let searchNumbers grid i j = 
    neighbours grid i j
    |> List.map( fun (x, y) -> (x, y, grid[x][y]))
    |> List.filter (fun (x, y, v) -> Char.IsDigit v)
    |> List.map (fun (x, y, v) -> findNumber grid x y)


let search (grid: char array array) = 
    let height = Array.length grid
    let width = Array.length grid[0]
    let mutable vals = []
    for i in 0 .. height - 1 do
        for j in 0 .. width - 1 do
            let s = grid[i][j]
            if not (Char.IsDigit s) && s <> '.'
            then 
                vals <- vals @ (searchNumbers grid i j) 
    vals

let searchGear (grid: char array array) = 
    let height = Array.length grid
    let width = Array.length grid[0]
    let mutable vals = []
    for i in 0 .. height - 1 do
        for j in 0 .. width - 1 do
            if grid[i][j] = '*'
            then 
                let neighbours = 
                    searchNumbers grid i j
                    |> List.distinct 
                if List.length neighbours = 2
                then
                    let (v1, _, _) = List.item 0 neighbours
                    let (v2, _, _) = List.item 1 neighbours
                    let prod = (int64 v1) * (int64 v2)
                    vals <- prod :: vals
    vals

let task1 =
    readFile ()
    |> List.map (fun l -> l |> Seq.toArray)
    |> List.toArray
    |> search 
    |> List.distinct 
    |> List.sumBy (fun (n, i, idx) -> n)
let task2 =
    readFile ()
    |> List.map (fun l -> l |> Seq.toArray)
    |> List.toArray
    |> searchGear
    |> List.sum

printfn $"Task 1: {task1}" // 517021
printfn $"Task 2: {task2}" // 81296995
