open System.IO

let readFile () = 
    File.ReadLines "input.txt"
    |> Seq.toArray
    |> Array.map (fun s -> Seq.toArray s)

let print grid = 
    grid
    |> Array.iter(fun row ->
            row |> Array.iter(fun c -> printf "%c" c)
            printfn ""
        )

type Direction = Left | Right | Up | Down

let outside grid (x, y) = 
    let height = Array.length grid
    let width = Array.length grid[0]
    x < 0 || x >= height || y < 0 || y >= width

let next (a, b) direction = 
    match direction with
    | Left -> (a,  b - 1)
    | Right -> (a, b + 1)
    | Up -> (a - 1, b)
    | Down -> (a + 1, b)

let nextDirection symbol direction = 
    match symbol, direction with
    | '.', _ -> [ direction ]
    | '|', Up ->  [ Up ]
    | '|', Down ->  [ Down ]
    | '|', Left ->  [ Up; Down]
    | '|', Right -> [ Up; Down]
    | '/', Up -> [ Right ] 
    | '/', Down -> [ Left ]
    | '/', Left -> [ Down ]
    | '/', Right ->  [ Up ]
    | '-', Left -> [ Left ]
    | '-', Right -> [ Right ]
    | '-', Up -> [ Left; Right]
    | '-', Down -> [ Left; Right]
    | '\\', Up -> [ Left ]
    | '\\', Down -> [ Right ]
    | '\\', Left -> [ Up ]
    | '\\', Right ->  [ Down ]
    | _ -> failwith "illegal char"

let rec move (grid: char array array) (visited: ((int * int) * Direction) Set) (current: int * int) (direction: Direction) : ((int * int) * Direction) Set = 
    let c = next current direction
    if outside grid c then Set.add (current, direction) visited
    else 
        let symbol = grid[fst c][snd c]
        let newVisited = Set.add (current, direction) visited
        let nextDir = nextDirection symbol direction
        nextDir
        |> List.filter (fun d -> Set.contains (c, d) visited |> not)
        |> List.fold (fun s dir -> move  grid s c dir) newVisited

let energize grid point direction = 
    move grid Set.empty point direction
    |> Set.map fst
    |> Set.filter (fun p -> not <| outside grid p)
    |> Set.count

let grid = readFile ()

let task1 = 
    energize grid (0, -1) Right

let task2 =
    let height = Array.length grid
    let width = Array.length grid[0]
    let top = [0 .. width - 1] |> List.map (fun i -> ((0, i - 1), Down))
    let bottom = [0 .. width - 1] |> List.map (fun i -> ((height - 1, i + 1), Up))
    let left = [0 .. height - 1] |> List.map (fun i -> ((i - 1, 0), Right))
    let right = [0 .. height - 1] |> List.map (fun i -> ((i + 1, width - 1), Left))
    top @ bottom @ left @ right 
    |> List.map (fun (p, d) -> energize grid p d )
    |> List.max

printfn $"Task 1: {task1}" // 7562
printfn $"Task 2: {task2}" // 7793
