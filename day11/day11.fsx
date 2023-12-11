open System.IO

let readFile () = 
    File.ReadLines "input.txt"
    |> Seq.toList

let  grid = 
        readFile ()
        |> List.map (fun l -> l |> Seq.toArray)
        |> List.toArray

let height = Array.length grid
let width = Array.length (grid[0])

let emptyColumns = 
    [0 .. width - 1]
    |> List.map(fun i -> (i, grid |> Array.map (fun row -> row[i]) |> Array.distinct))
    |> List.filter (fun (i, a) -> Array.length a = 1)
    |> List.map fst

let emptyRows =
    grid
    |> Array.mapi (fun i row -> if row |> Array.distinct |> Array.length = 1 then Some i else None)
    |> Array.choose id
    |> Array.toList

let updateEmptyRowsAndCols grid = 
    let gridUpdatedColumns =
        grid 
        |> Array.map (fun row ->
                row
                |> Array.mapi (fun i c -> if List.contains i emptyColumns then [|'.'; '.'|] else [| c |])
                |> Array.collect id)

    gridUpdatedColumns
    |> Array.mapi (fun i row -> if List.contains i emptyRows then [| row; row |] else [| row |])
    |> Array.collect id
    
let galaxies grid =
    grid
    |> Array.mapi (fun i row -> Array.mapi (fun j c -> ((i, j), c)) row)
    |> Array.collect id
    |> Array.filter (fun (p, c) -> c = '#')
    |> Array.map fst

let numEmptyColumns a b =
    let min = min a b
    let max = max a b 
    [min .. max]
    |> List.filter (fun x -> List.contains x emptyColumns)
    |> List.length

let numEmptyRows a b =
    let min = min a b
    let max = max a b 
    [min .. max]
    |> List.filter (fun x -> List.contains x emptyRows)
    |> List.length
    
let distances factor galaxies =
    galaxies
    |> Array.collect (fun p -> Array.map (fun q -> (p,  q)) galaxies)
    |> Array.filter (fun (p, q) -> p <> q)
    |> Array.map (fun ((x, y),(a, b)) -> (int64 (abs(a - x) + (abs (b - y))) + (factor - 1L) * (int64)(numEmptyRows a x + numEmptyColumns b y)))
    |> Array.sum
    |> fun i -> i / 2L 

let task1 =
    grid
    |> updateEmptyRowsAndCols
    |> galaxies
    |> distances 1L
let task2 =
    grid
    |> galaxies
    |> distances 1000000L

printfn $"Task 1: {task1}" // 9599070
printfn $"Task 2: {task2}" // 842645913794
