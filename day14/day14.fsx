open System.Data
open System.IO

type Direction = | North | West | South | East

let readFile () = 
    File.ReadLines "input.txt"
    |> Seq.toArray
    |> Array.map (Seq.toArray)

let print grid = 
    grid
    |> Array.iter(fun row ->
            row |> Array.iter(fun c -> printf "%c" c)
            printfn ""
        )

let rec moveRocks (prev: char array) (row: char array) = 
    if prev = row then row
    else
        let stoneIdxs = 
            row
            |> Array.mapi (fun i c -> (i, c))
            |> Array.filter (fun (_, c) -> c ='O')
            |> Array.map fst
        let newRow = 
            stoneIdxs
            |> Array.fold (fun (arr: char array) i -> 
                if i - 1 >= 0 && arr[i - 1] = '.' then
                    arr
                    |> Array.updateAt (i - 1) 'O'
                    |> Array.updateAt i '.'
                else arr) row
        moveRocks row newRow

let rotate (direction: Direction) width height (grid: char array array) =
    match direction with
    | North ->
        [|0 .. width - 1|]
        |> Array.map (fun idx -> 
            [|0 .. height - 1|]
            |> Array.map (fun i -> grid[i][idx]))
    | West -> grid
    | South ->
        [|0 .. width - 1|]
        |> Array.map (fun idx -> 
            [|0 .. height - 1|]
            |> Array.map (fun i -> grid[i][idx]) |> Array.rev) |> Array.rev
    | East ->
        grid
        |> Array.map Array.rev    

let rotateBack (direction: Direction) width height (grid: char array array) =
    match direction with
    | North -> rotate North width height grid
    | West -> grid
    | South -> rotate South width height grid
    | East -> rotate East width height grid
    

let northSupportBeams width height (grid: char array array) =
    let size = Array.length grid[0]
    grid
    |> rotate North width height
    |> Array.map (
        fun row -> 
            row 
            |> Array.mapi (fun i c -> if c = 'O' then int64 (size - i) else 0L)
            |> Array.sum
            )
    |> Array.sum

let move (direction: Direction) (grid: char array array) = 
    let width = Array.length grid[0]
    let height = Array.length grid 
    let moved = 
        rotate direction width height grid
        |> Array.map (fun row -> moveRocks [||] row)
    
    let back = rotateBack direction width height moved
    (northSupportBeams width height back, back)

let grid = readFile ()

let cycle = [North; West; South; East]

let moveCycle (grid: char array array) =
    cycle
    |> List.fold (fun (_, snd) d ->
        move d snd
        ) (0L, grid)

let rec findCycle (idx: int) (beams: (int * int64) list) (grids: (int * char array array) list) (grid: char array array) =
    let (beam, gr) = moveCycle grid
    let found = List.tryFind (fun (i, g) -> g = gr) grids
    match found with
    | Some (i, g) -> 
        let correct = (1000000000 - i) % (idx - i) + i
        beams
        |> List.find (fun (i, g) -> i = correct)
        |> snd
    | None ->
        findCycle (idx + 1) ((idx, beam) :: beams) ((idx, gr) :: grids) gr 
    
let task1 =  move North grid |> fst
let task2 = findCycle 1 [] [] grid

printfn $"Task 1: {task1}" // 110677
printfn $"Task 2: {task2}" // 90551