open System.IO

let readFile () = 
    File.ReadLines "input.txt"
    |> Seq.toList

let parse (lines: string list) = 
    let instructions = lines |> List.head |> Seq.toList
    let map = 
        lines
        |> List.skip 2
        |> List.fold (fun m l ->
            let parts = l.Split(" = ")
            let key = parts[0]
            let fst = parts[1].Substring(1, 3)
            let snd = parts[1].Substring(6, 3)
            Map.add key (fst, snd) m
        ) Map.empty
    (instructions, map)

let rec search (criteria: string -> bool) instructions map (key: string) count idx =
    if criteria key then count
    else
        let next = Map.find key map
        let instruction = List.item idx instructions
        let newKey = if instruction = 'L' then fst next else snd next
        let newIdx = if idx + 1 >= List.length instructions then 0 else idx + 1
        search criteria instructions map newKey (count + 1) newIdx

let rec gcd x y = if y = 0L then abs x else gcd y (x % y)
let lcm x y = x * y / (gcd x y)

let search2 criteria instructions map keys count idx =
    keys 
    |> List.map (fun k -> search criteria instructions map k 0 0)
    |> List.fold (fun acc i ->  lcm acc (int64 i) ) 1L

let task1 =
    let (instructions, map) = 
        readFile ()
        |> parse
    search (fun s -> s = "ZZZ") instructions map "AAA" 0 0

let task2 = 
    let (instructions, map) = readFile () |> parse
    let startKeys = map |> Map.keys |> Seq.filter (fun k -> k.EndsWith("A")) |> Seq.toList
    search2 (fun s -> s.EndsWith("Z")) instructions map startKeys 0 0

printfn $"Task 1: {task1}"
printfn $"Task 2: {task2}"
