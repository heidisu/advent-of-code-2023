open System.IO

let readFile () = 
    File.ReadLines "input.txt"
    |> Seq.toList

let nextStep  visited candidates = 
    candidates
    |> List.filter (fun i -> not <| List.contains i visited)
    |> List.head 

let next (grid: char array array) (visited: (int * int) list) (x, y) =
    match grid[x][y] with
    | '|' -> nextStep visited [(x - 1, y); (x + 1, y)]
    | 'J' -> nextStep visited [(x, y - 1); (x - 1, y)]
    | '7' -> nextStep visited [(x, y - 1); (x + 1, y)]
    | 'L' -> nextStep visited [(x - 1, y); (x, y + 1)]
    | 'F' -> nextStep visited [(x + 1, y); (x, y + 1)]
    | '-' -> nextStep visited [(x, y - 1);(x, y + 1)]
    | _ -> failwith "uexpected value"


let rec move (grid: char array array) visited steps left right =
    if left  = right then (steps, left :: visited)
    else 
        let newLeft = next grid visited left
        let newRight = next grid visited right
        move grid (left :: right :: visited) (steps + 1) newLeft newRight

let findStart grid = 
    grid
    |> Array.mapi (fun i r -> r |> Array.mapi (fun j v -> ((i, j), v)))
    |> Array.collect id
    |> Array.find (fun (_, v) -> v = 'S')
    |> fst

let inGrid grid (x, y) = 
    let height = Array.length grid
    let widht = Array.length grid[0]
    x >= 0 && x < height && y >= 0 && y < widht

let findStartNeighbours (grid: char array array) (x, y) = 
    let mutable neighbours = []
    
    if inGrid grid (x + 1, y) then // down
        match grid[x + 1][y] with
        | '|'
        | 'L'
        | 'J' ->  neighbours <- (x + 1, y) :: neighbours
        | _ -> ()

    if inGrid grid (x - 1, y) then // up
        match grid[x - 1][y] with 
        | '|'
        | 'F'
        | '7' -> neighbours <- (x - 1, y) :: neighbours
        | _ -> ()
    
    if inGrid grid (x, y - 1) then // left
        match grid[x][y - 1] with
        | '-'
        | 'L'
        | 'F' ->  neighbours <- (x, y - 1) :: neighbours
        | _ -> ()

    if inGrid grid (x, y + 1) then // right
        match grid[x][y + 1] with 
        | '-'
        | '7'
        | 'J' -> neighbours <- (x, y + 1) :: neighbours
        | _ -> ()

    if List.length neighbours <> 2 then failwith "invalid start"
    (List.head neighbours, List.last neighbours)

let  grid = 
        readFile ()
        |> List.map (fun l -> l |> Seq.toArray)
        |> List.toArray
let start = findStart grid
let (left, right) = findStartNeighbours grid start
let (steps, path) = move grid  [start] 1 left right

let task1 = steps
printfn $"Task 1: {task1}" // 6640

let isNeighbour ((x, y): int * int) ((a, b): int * int) = 
    (x = a + 1 && y = b) || (x = a - 1 && y = b) || (x = a && y = b + 1) || (x = a && y = b - 1)

let rec findNeighbours points (current : (int * int) Set) (prev: (int * int) Set) = 
    if current = prev then current
    else 
        let newNeighbours =
            points 
            |> List.filter (fun p -> current |> Set.exists (fun q -> isNeighbour p q))
            |> Set.ofList
        let newCurrent = Set.union newNeighbours current
        findNeighbours points newCurrent current

let rec groupPoints (acc: (int * int) Set Set) (points : (int * int) List) (current: (int * int) Set) =
    if List.isEmpty points then acc
    else 
        let newGroup = findNeighbours points current Set.empty
        let newPoints = points |> List.filter (fun p -> not <| Set.contains p newGroup)
        if List.isEmpty newPoints then Set.add newGroup acc
        else 
            let newCurrent = Set.add (List.head newPoints) Set.empty
            groupPoints (Set.add newGroup acc) (List.tail newPoints) newCurrent

let isOutside (path: (int * int) list) (group: Set<int * int>) = 
    let (x, y) = group |> Set.toList |> List.head
    let intersections = path |> List.filter (fun (a, b) -> a = x && b > y)
    let horisontalSegmentsRemoved = 
        intersections
        |> List.sortBy snd
        |> List.filter (fun (x, y) -> grid[x][y] <> '-')
        
    let numberSteps = 
        horisontalSegmentsRemoved        
        |> List.windowed 2 
        |> List.filter (fun l -> 
            let (x, y) = l[0]
            let (a, b) = l[1]
            (grid[x][y] = 'F' && grid[a][b] = 'J') || grid[x][y] = 'L' && grid[a][b] = '7'
            )
        |> List.length
    (List.length horisontalSegmentsRemoved - numberSteps) % 2 = 0

let task2 = 
    let points = 
        grid 
        |> Array.mapi (fun i row -> Array.mapi (fun j _ -> (i, j)) row)
        |> Array.collect id
        |> Array.filter (fun (x, y) -> not <| List.contains (x,y) path)
    
    let first = Set.empty |> Set.add (Array.head points)
    let rest = points |> Array.tail |> Set.ofArray |> Set.toList

    let groups = groupPoints Set.empty rest first
    let insides =
        groups
        |> Set.filter (fun s -> not <| isOutside (List.distinct path) s)
    
    insides
    |> Set.toList
    |> List.map (fun s -> Set.count s)
    |> List.sum

printfn $"Task 2: {task2}" // 411
