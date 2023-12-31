open System.IO

type Range = {
    Source: int64
    Destination: int64
    Range: int64
}   

type AlmanacRange = {
    From: string
    To: string
    Ranges: Range list
}

type Almanac = {
    Seeds: int64 list
    Maps: AlmanacRange list
}

type Seed = {
    Start: int64
    Length: int64
}
type Almanac2 = {
    Seeds: Seed list
    Maps: AlmanacRange list
}

let readFile () = 
    File.ReadLines "test-input.txt"
    |> Seq.toList

let parse (lines: string list): Almanac =
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
                | Some almanac -> 
                    let sorted = { almanac with Ranges = almanac.Ranges |> List.sortBy (fun r -> r.Source)}
                    (almanac :: maps, None)
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

let searchValue (source: int64) (range: Range) = 
    if source >= range.Source && source < range.Source + range.Range
    then
        let idx = (source - range.Source) |> int64
        Some <| range.Destination + idx
    else None

let intersection (s: Seed) (r: Range): Range option = 
    if s.Start >= r.Source + r.Range || s.Start + s.Length - 1L < r.Source
    then None
    else 
        let start = max (r.Source) (s.Start)
        let stop = min (s.Start + s.Length - 1L) (r.Source + r.Range - 1L)
        let destIdx = (start - r.Source)
        let dest = r.Destination + destIdx
        let range = { Source = start; Range = stop - start + 1L; Destination = dest }
        Some range

let searchRange (s: Seed) (ranges: Range list) : Seed list = 
    let intersections = 
        ranges
        |> List.choose (intersection s)
        |> List.sortBy (fun r -> r.Source)
    if List.isEmpty intersections then [ s ]
    else 
        let first = List.head intersections
        let startList =
            if s.Start < first.Source then [first; {
                Source = s.Start
                Destination = s.Start
                Range = first.Source - s.Start

            }] else [ first ]
        let (complete, last) = 
            intersections
            |> List.tail
            |> List.fold (fun (l, prev) r ->
                    let gapStart = prev.Source + prev.Range
                    if gapStart < r.Source then 
                        (r :: {
                            Source = gapStart
                            Destination = gapStart
                            Range = r.Source - gapStart
                        } :: l, r)
                    else (r :: l, r)
                ) (startList, first)
        let total = 
            if s.Start + s.Length > last.Source + last.Range 
            then 
                let start = last.Source + last.Range
                let length = s.Start + s.Length - start
                { Source = start; Destination = start; Range = length  } :: complete 
            else complete 
        total
        |> List.map (fun r -> 
            { Start = r.Destination; Length = r.Range })     

let toAlmanac2 (a: Almanac): Almanac2 =
    {
        Seeds =
            a.Seeds
            |> List.chunkBySize 2
            |> List.map (fun l -> { Start = List.head l; Length = List.item 1 l})
        Maps = a.Maps
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
                |> List.choose (fun r -> searchValue s r)
            match matches with
            | [] -> s
            | [x] -> x
            | _ -> failwith "should not be more than one"
        )
    ) (almanac.Seeds)
    |> List.min

let task2 =
    let almanac = 
        readFile ()
        |> parse
        |> toAlmanac2
    almanac.Maps
    |> List.fold (fun (sl: Seed list) (m: AlmanacRange)  -> 
        sl
        |> List.collect (fun (s: Seed) -> 
            searchRange s m.Ranges
        )
    ) (almanac.Seeds)
    |> List.minBy (fun s -> s.Start)
    |> (fun s -> s.Start)

printfn $"Task 1: {task1}" // 600279879
printfn $"Task 2: {task2}" // 20191102
