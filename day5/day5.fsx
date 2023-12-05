open System.IO

type AlmanacMap = {
    From: string
    To: string
    Map: Map<int64, int64>
}


type Almanac = {
    Seeds: int64 list
    Maps: AlmanacMap list
}

let readFile () = 
    File.ReadLines "test-input.txt"
    |> Seq.toList

let parse (lines: string list) =
    let seeds = 
        lines 
        |> List.head
        |> fun line -> 
                let numbers = line.Split("seeds: ")[1]
                numbers.Split(" ") |> Array.map int64 
        |> Array.toList
    let (maps, current) = 
        lines
        |> List.skip 2
        |> List.fold ( fun (maps: AlmanacMap list, current: AlmanacMap option) l ->
            if l = "" then  
                match current with
                | None -> (maps, None)
                | Some almanac -> (almanac :: maps, None)
            else 
                match current with
                | None ->
                    let parts = l.Split(" ")[0]
                    let names = parts.Split("-to-")
                    let from = names[0]
                    let toName = names[1]
                    let current = {
                        From = from
                        To = toName
                        Map = Map.empty
                    }
                    (maps, Some current)
                | Some almanac ->
                    let parts = 
                        l.Split(" ")
                        |> Array.map int64
                    let range = parts[2]
                    let sourceRange = [parts[1] .. parts[1] + range - (int64 1)]
                    let destRange = [parts[0] .. parts[0] + range - (int64 1)]
                    let newMap = 
                        List.zip sourceRange destRange
                        |> List.fold (fun m (s, d) -> Map.add s d m) almanac.Map
                    (maps, Some { almanac with Map = newMap})
        ) (List.empty, None)
    let finalMaps = 
        match current with
        | None -> maps
        | Some m -> m :: maps   
    {
        Seeds = seeds
        Maps = List.rev finalMaps
    }

let seedToLocation (a: Almanac) (currIdx: int) = 
    

let task1 = 
    let almanac = 
        readFile ()
        |> parse
    (* almanac.Maps
    |> List.fold (fun s m  -> 
        match m with
        | [] -> s
        | x :: xs -> 
            List.map
    ) (almanac.Seeds)*)
let task2 = "task 2"

printfn $"Task 1: {task1}"
printfn $"Task 2: {task2}"
