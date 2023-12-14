
open System.IO

let readFile () = 
    File.ReadLines "input.txt"
    |> Seq.toList

let parseSingleMap (lines: string list) = 
    lines 
    |> List.map (fun s -> s |> Seq.toArray)
    |> List.toArray

let parseMaps (lines: string list): char array array list = 
    let (acc, curr) = 
        lines
        |> List.fold (fun (acc, curr) l -> 
            if l.Trim() = "" then
                let map = curr |> List.rev  |> parseSingleMap
                (map :: acc, [])
            else (acc, l :: curr)
         ) ([], [])
    let lastMap = curr |> List.rev |> parseSingleMap
    lastMap :: acc     

let searchSymmetricIndex idx (map: char array array) = 
    let size = Array.length map
    let pairs = 
        [0 .. size - 1]
        |> List.map (fun j -> (idx - j, idx + j + 1))
        |> List.filter (fun (a, b) -> a >= 0 && a < size && b >= 0 && b < size)
    pairs 
    |> List.forall (
        fun (a, b) -> 
            map[a] = map[b])

let searchFuzzySymmetricIndex idx (map: char array array) = 
    let size = Array.length map
    let pairs = 
        [0 .. size - 1]
        |> List.map (fun j -> (idx - j, idx + j + 1))
        |> List.filter (fun (a, b) -> a >= 0 && a < size && b >= 0 && b < size)
    pairs 
    |> List.map (
        fun (a, b) -> 
            let zipped = Array.zip map[a] map[b]
            zipped 
            |> Array.map (fun (a, b) -> if a = b then 0 else 1)
            |> Array.sum
            )
    |> List.sum 
    |> (fun i -> i = 1)

let searchSymmetry (indexSearch: int -> char array array -> bool) (map: char array array) = 
    let width = Array.length map[0]
    let height = Array.length map
    let columns = 
        [|0 .. width - 1|]
        |> Array.map (fun idx -> 
            [|0 .. height - 1|]
            |> Array.map (fun i -> map[i][idx]))

    let columnSymmetry = 
        [0 .. width - 2] 
        |> List.map (fun i -> (i, indexSearch i columns)) 
        |> List.filter (fun (i, b) -> b = true )
   
        
    let colNum =
        if List.isEmpty columnSymmetry then 0L
        else
            columnSymmetry
            |> List.map fst
            |> List.max
            |> int64
            |> (+) 1L
    let height = Array.length map 
    let rowSymmetry = 
        [0 .. height - 2] 
        |> List.map (fun i -> (i, indexSearch i map)) 
        |> List.filter (fun (i, b) -> b = true )
        
    let rowNum =
        if List.isEmpty rowSymmetry then 0L
        else
            rowSymmetry
            |> List.map fst
            |> List.max
            |> fun n -> ((int64 n) + 1L) * 100L
    if rowNum > 0 then rowNum else colNum

let maps = 
    readFile()
    |> parseMaps
    |> List.rev

let task1 =
    maps
    |> List.map (searchSymmetry searchSymmetricIndex)
    |> List.sum

let task2 =
    maps
    |> List.map (searchSymmetry searchFuzzySymmetricIndex)
    |> List.sum

printfn $"Task 1: {task1}" // 34202
printfn $"Task 2: {task2}" // 34230
