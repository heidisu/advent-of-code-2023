open System.IO

let readFile () = 
    File.ReadLines "input.txt"
    |> Seq.toList

let findNeighbours (x, y) (grid: char array array) =
    match grid[x][y] with
    | '>' -> [(x, y + 1)]
    | '<' -> [(x, y - 1)]
    | 'v' -> [(x + 1, y)]
    | '^' -> [(x - 1, y)]
    | '.' ->
        [(x + 1, y); (x - 1, y); (x, y + 1); (x, y - 1)]
        |> List.filter (fun (a, b) -> a >= 0 && a < Array.length grid && b >= 0 && b < Array.length grid[0])
        |> List.filter (fun (a, b) -> grid[a][b] <> '#')
    | _ -> failwith "illegal character"

let findNeighbours2 (x, y) (grid: char array array) =
    match grid[x][y] with
    | '>'
    | '<'
    | 'v'
    | '^'
    | '.' ->
        [(x + 1, y); (x - 1, y); (x, y + 1); (x, y - 1)]
        |> List.filter (fun (a, b) -> a >= 0 && a < Array.length grid && b >= 0 && b < Array.length grid[0])
        |> List.filter (fun (a, b) -> grid[a][b] <> '#')
    | _ -> failwith "illegal character"


let rec longestPath (neighbours: Map<int * int, (int * int) list>) (dist: int64[,]) (visited: bool[,]) p currSum =
    let (x, y) = p
    if visited[x,y] then ()
    else 
        visited[x,y] <- true
        if dist[x,y] < currSum then dist[x,y] <- currSum
        neighbours
        |> Map.find p
        |> List.iter (fun i ->
            longestPath neighbours dist visited i (currSum + 1L)
        )
        visited[x, y] <- false

let grid = 
    readFile()
    |> List.map (fun s -> s |> Seq.toArray)
    |> List.toArray
let height = Array.length grid
let width = Array.length grid[0]
let start = 
        grid[0] 
        |> Array.mapi (fun i c -> (i, c))
        |> Array.find (fun (i, c) -> c = '.')
        |> fun (i, _) -> (0, i)
let goal = 
    grid[height - 1] 
    |> Array.mapi (fun i c -> (i, c))
    |> Array.find (fun (i, c) -> c = '.')
    |> fun (i, _) -> (height - 1, i)

let candidates = 
    grid
    |> Array.mapi (fun i r -> r |> Array.mapi (fun j c -> ((i, j), c)))
    |> Array.collect id
    |> Array.filter (fun (x, c) -> c <> '#')
    |> Array.map fst
    |> List.ofArray

let findLongestPath findNeighbours = 
    let dist = Array2D.create height width -1L
    dist[fst start, snd start] <-0L
    let visited = Array2D.create height width false
    let neighbours =
        candidates
        |> List.fold (fun acc p -> 
            let nbs = findNeighbours p grid
            Map.add p nbs acc
        ) Map.empty
    longestPath neighbours dist visited start 0
    dist[fst goal,snd goal]

let task1 =
    findLongestPath findNeighbours

let task2 = 
    findLongestPath findNeighbours2

printfn $"Task 1: {task1}" // 2094
printfn $"Task 2: {task2}" // 6442 - very slow ~ 24 hrs
