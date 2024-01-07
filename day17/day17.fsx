open System.IO

let readFile () = 
    File.ReadLines "input.txt"
    |> Seq.toArray
    |> Array.map (fun r -> r |> Seq.map (fun c -> c |> string |> int64) |> Seq.toArray)

let grid = readFile ()
let height = Array.length grid
let width = Array.length grid[0]

let print grid = 
    grid
    |> Array.iter(fun row ->
            row |> Array.iter(fun c -> printf "%i" c)
            printfn ""
        )

type Node = {
    Point: int * int
    Direction: int * int
    OnLine: int
}

let inGrid (x, y) = 
    x >= 0 && x < height && y >=0 && y < width 

let leftRight (x,  y) = 
    match(x, y) with
    | (0, 1)
    | (0, -1) -> [(1, 0); -1, 0]
    | (1, 0)
    | (-1, 0) -> [(0, -1); (0, 1)]
    | _ -> failwith "illegal distance"

let findNeighbours (n: Node) = 
    let (x, y) = n.Point
    let lr  = leftRight n.Direction
    let directions = if n.OnLine < 3 then n.Direction :: lr else lr
    directions
    |> List.map (fun (a, b) -> {
        Point = (x + a, y + b)
        Direction = (a, b)
        OnLine = if (a, b) = n.Direction then n.OnLine + 1 else 1
    } )
    |> List.filter (fun n -> inGrid n.Point)

let findNeighbours2 (n: Node) = 
    let (x, y) = n.Point
    let lr  = leftRight n.Direction
    let directions = 
        if n.OnLine < 4 then [ n.Direction ] 
        else if n.OnLine < 10 then n.Direction :: lr
        else lr
    directions
    |> List.map (fun (a, b) -> {
        Point = (x + a, y + b)
        Direction = (a, b)
        OnLine = if (a, b) = n.Direction then n.OnLine + 1 else 1
    } )
    |> List.filter (fun n -> inGrid n.Point)

let rec dijkstra (getNeighbours: Node -> List<Node>) (dist: Map<Node, int64>) (prev: Map<Node, Node>) (notVisisted: Set<Node>) (destination: int * int) (grid: int64 array array) = 
    if Set.isEmpty notVisisted then (dist, prev)
    else 
        let (u, currVal) = 
            notVisisted
            |> Set.map (fun n  ->
                    (n, Map.find n dist))
            |> Set.toList
            |> List.minBy (fun (p, m) -> m)
        let (newDist, newPrev, newNotVisited) = 
            getNeighbours u
            |> List.fold (fun (dist, prev, nvis) n -> 
                let (i, j) = n.Point
                let alt = grid[i][j] + currVal
                match Map.tryFind n dist with
                | Some d -> 
                    if alt < d then 
                        (Map.add n  alt dist, Map.add n u prev, Set.add n nvis)
                    else (dist, prev, nvis)
                | None -> 
                    (Map.add n alt dist, Map.add n u prev, Set.add n nvis)
                ) (dist, prev, notVisisted)
        dijkstra getNeighbours newDist newPrev (Set.remove u newNotVisited) destination grid

let rec findPath prev n = 
    if n.Point = (0,0) then [(0,0)]
    else n.Point :: findPath prev (Map.find n prev)

let start = {
    Point = (0,0)
    Direction = (0,0)
    OnLine = 0 
}

let p1 = {
    Point = (0, 1)
    Direction = (0, 1)
    OnLine = 1 
}

let p2 = {
    Point = (1, 0)
    Direction = (1, 0)
    OnLine = 1 
}

let notVisited = Set.empty |> Set.add p1 |> Set.add p2
let dist =
    Map.empty
    |> Map.add start 0L
    |> Map.add p1 (grid[fst p1.Point][snd p1.Point])
    |> Map.add p2 (grid[fst p2.Point][snd p2.Point])
let prev =
    Map.empty
    |> Map.add p1 start
    |> Map.add p2 start

let task1 =
    let (dist, prev) = dijkstra findNeighbours dist prev notVisited (height - 1, width - 1) grid
    dist
    |> Map.filter (fun k v -> k.Point = (height - 1, width - 1)) 
    |> Map.toList
    |> List.minBy snd
    |> snd

let task2 =
    let (dist, prev) = dijkstra findNeighbours2 dist prev notVisited (height - 1, width - 1) grid
    dist
    |> Map.filter (fun k v -> k.Point = (height - 1, width - 1) && k.OnLine > 3) 
    |> Map.toList
    |> List.minBy snd
    |> snd

printfn $"Task 1: {task1}" // 665
printfn $"Task 2: {task2}" // 809