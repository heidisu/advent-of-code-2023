open System.IO

type Range = {
    Source: int64
    Destination: int64
    Range: int64
}

let getValue (source: int64) (range: Range) = 
    if source >= range.Source && source < range.Source + range.Range
    then
        let idx = (source - range.Source) |> int64
        Some <| range.Destination + idx
    else None
        

type AlmanacRange = {
    From: string
    To: string
    Ranges: Range list
}

type Almanac = {
    Seeds: int64 list
    Maps: AlmanacRange list
}

let readFile () = 
    File.ReadLines "input.txt"
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
        |> List.fold ( fun (maps: AlmanacRange list, current: AlmanacRange option) l ->
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
                        Ranges = List.empty
                    }
                    (maps, Some current)
                | Some almanac ->
                    let parts = 
                        l.Split(" ")
                        |> Array.map int64
                    let range = parts[2]
                    let source = parts[1]
                    let destination = parts[0]
                    let newRange = { Source =  source; Destination = destination; Range = range }
                        
                    (maps, Some { almanac with Ranges = newRange :: almanac.Ranges})
        ) (List.empty, None)
    let finalMaps = 
        match current with
        | None -> maps
        | Some m -> m :: maps   
    {
        Seeds = seeds
        Maps = List.rev finalMaps
    }
    
let task1 = 
    let almanac = 
        readFile ()
        |> parse
    almanac.Maps
    |> List.fold (fun s m  -> 
        s
        |> List.map (fun s ->
            let matches =
                m.Ranges
                |> List.choose (fun r -> getValue s r)
            match matches with
            | [] -> s
            | [x] -> x
            | _ -> failwith "should not be more than one"
        )
    ) (almanac.Seeds)
    |> List.min
let task2 = "task 2"

printfn $"Task 1: {task1}"
printfn $"Task 2: {task2}"
