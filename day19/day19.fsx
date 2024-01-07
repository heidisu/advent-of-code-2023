open System.IO

let readFile () = 
    File.ReadLines "input.txt"
    |> Seq.toList


type Part = {
    X: int64
    M: int64
    A: int64
    S: int64
}

type State = Accepted | Rejected

type Result = State of State | JobName of string

type Variable = X | M | A | S

type Instruction = {
    Id: string
    Condition: (Variable * (Part -> bool)) option
    Result: Result
}

type Job = {
    Name: string
    Instructions: Instruction list
}

let parseResult (s: string): Result = 
    match s with
    | "A" -> State Accepted
    | "R" -> State Rejected
    | s -> JobName s

let filter (p: Part) (s: string): int64 = 
    match s with
    | "s" -> p.S
    | "x" -> p.X
    | "a" -> p.A
    | "m" -> p.M
    | _ -> failwith "illegal string"

let parseVariable (s: string) = 
    match s with
    | "x" -> X
    | "m" -> M
    | "a" -> A
    | "s" -> S
    | _ -> failwith "illegal variable name"

let parseCondition (s: string): (Variable * (Part -> bool)) = 
    if s.Contains("<") then
        let parts = s.Split("<")
        let name = parts[0]
        let limit = parts[1] |> int64
        (parseVariable name, fun (p: Part) -> filter p name < limit)
    else 
        let parts = s.Split(">")
        let name = parts[0]
        let limit = parts[1] |> int64
        (parseVariable name, fun (p: Part) -> filter p name > limit)

let parse (lines: string list) = 
    let jobLines = 
        lines |> List.takeWhile (fun l -> l <> "")
    let partLines = 
        lines |> List.skip ((List.length jobLines) + 1)
    let jobs = 
        jobLines
        |> List.map(fun l -> l.Replace("}", "").Split("{"))
        |> List.map (fun a -> 
                let jobName = a[0]
                let instructions = 
                    a[1].Split(",")
                    |> Array.mapi (fun i s -> 
                        if s.Contains(":") then
                            let parts = s.Split(":")
                            {
                                Id = sprintf "%s-%d" jobName i
                                Condition = Some (parseCondition parts[0])
                                Result = parseResult (parts[1])
                            }
                        else
                            {
                                Id = sprintf "%s-%d" jobName i
                                Condition = None
                                Result = parseResult s
                            }
                    ) |> Array.toList
                {
                    Name = jobName
                    Instructions = instructions
                }
        )

    let parts = 
        partLines
        |> List.map (fun s -> s.Replace("{", "").Replace("}", ""))
        |> List.map (fun s -> s.Split(","))
        |> List.map (fun a -> 
                a 
                |> Array.map (fun s -> s.Split("=") |> fun a -> (a[0], int64 a[1]))
                |> Map.ofArray)
        |> List.map (fun m -> { 
                        A = Map.find "a" m
                        M = Map.find "m" m
                        X = Map.find "x" m
                        S = Map.find "s" m
                        })
    let jobMap = 
        jobs 
        |> List.map (fun j -> (j.Name, j))
        |> Map.ofList
    (jobMap, parts)

let rec doWork (jobMap: Map<string, Job>) (part: Part) jobName: State = 
    let job = Map.find jobName jobMap
    let instruction = 
        job.Instructions
        |> List.find (fun i -> 
            match i.Condition with 
            | Some (n, c) -> c part
            | None -> true
            )
    match instruction.Result with
    | JobName name -> doWork jobMap part name
    | State state -> state

let rec searchBackwards (allInstructions: (string * Instruction) list) (current: (string * Instruction) list list)= 
    let candidates = 
        current
        |> List.map (fun l ->
            let (name, i) = List.head l
            let prevs = 
                allInstructions
                |> List.filter (fun (n, i) -> i.Result = JobName name)
            (l, prevs))
    let allPrevs = 
        candidates
        |> List.collect snd
    if List.isEmpty allPrevs then current
    else 
        let newCurrent = 
            candidates
            |> List.collect (fun (l, prevs) -> 
                
                match prevs with
                | [] -> [l]
                | prevs -> 
                    prevs
                    |> List.map (fun p -> p :: l)
            )
        searchBackwards allInstructions newCurrent 

let filterForInstruction (instruction: Instruction) (negate: bool) (x: int64 list, m : int64 list, a: int64 list, s: int64 list) = 
    match instruction.Condition with
    | Some(X, c) -> 
        let conditionFunc = if negate then c >> not else c
        let xRes = x |> List.filter (fun i -> conditionFunc {X = i; M = 0; A = 0; S = 0}) 
        (xRes, m, a, s)
    | Some(M, c) -> 
        let conditionFunc = if negate then c >> not else c
        let mRes = m |> List.filter (fun i -> conditionFunc {X = 0; M = i; A = 0; S = 0}) 
        (x, mRes, a, s)
    | Some(A, c) -> 
        let conditionFunc = if negate then c >> not else c
        let aRes = a |> List.filter (fun i -> conditionFunc {X = 0; M = 0; A = i; S = 0}) 
        (x, m, aRes, s)
    | Some(S, c) -> 
        let conditionFunc = if negate then c >> not else c
        let sRes = s |> List.filter (fun i -> conditionFunc {X = 0; M = 0; A = 0; S = i}) 
        (x, m, a, sRes)
    | None -> (x, m, a, s)

let findCombinationsForInstruction (job: Job)  (instruction: Instruction) (x: int64 list, m : int64 list, a: int64 list, s: int64 list) = 
    let (x, m, a, s) = 
        job.Instructions
        |> List.takeWhile (fun i -> i.Id <> instruction.Id)
        |> List.fold (fun (x, m, a, s) i -> 
            filterForInstruction i true (x, m, a, s)
        ) (x, m, a, s)
    filterForInstruction instruction false (x, m, a, s)

let rec findCombinations (jobMap: Map<string, Job>) (x: int64 list, m : int64 list, a: int64 list, s: int64 list) (path: (string * Instruction) list) = 
    match path with
    |  [] -> (x, m, a, s)
    | (jobname, i) :: xs -> 
        let job = Map.find jobname jobMap
        let newRanges = findCombinationsForInstruction job i (x, m, a, s)
        findCombinations jobMap newRanges xs
let task1 = 
    let (jobMap, parts) = 
        readFile ()
        |> parse
    parts
    |> List.map (fun p -> (p, doWork jobMap p "in"))
    |> List.filter (fun (p, s) -> s = Accepted)
    |> List.map (fun (p, _) -> p.X + p.M + p.S + p.A)
    |> List.sum
let task2 =
    let (jobMap, _) = 
        readFile ()
        |> parse
    let jobs = Map.values jobMap |> Seq.toList
    let allInstructions =
        jobs
        |> List.collect (fun j -> j.Instructions |> List.map (fun i -> (j.Name, i))) 
    let withAccpeted = 
        allInstructions
        |> List.filter (fun (j, i) -> i.Result = State Accepted)
        |> List.map (fun l -> [l])

    searchBackwards allInstructions withAccpeted
    |> List.map (fun l -> findCombinations jobMap ([1 .. 4000], [1 .. 4000], [1 .. 4000], [1 .. 4000]) l)
    |> List.map (fun (x, m, a, s) -> (int64 <| List.length x) * (int64 <| List.length m) * (int64 <| List.length a) * (int64 <| List.length s))
    |> List.sum

printfn $"Task 1: {task1}" // 418498
printfn $"Task 2: {task2}" // 123331556462603
