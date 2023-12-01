open System.IO
open System

let readFile () = 
    File.ReadLines "input.txt"
    |> Seq.toList

(*
let task1 =
    readFile ()
    |> List.map (fun s ->
        let digits = 
            s 
            |> Seq.filter(fun c -> Char.IsDigit(c)) 
            |> Seq.toList
        (int <| Char.ToString(List.head digits)) * 10 + (int <| Char.ToString(List.last digits))
        )
    |> List.sum    
*)
let replaceStringDigits (s: String) = 
    s
        .Replace("zero", "0")
        .Replace("one", "1")
         .Replace("two", "2")
          .Replace("three", "3")
           .Replace("four", "4")
            .Replace("five", "5")
             .Replace("six", "6")
              .Replace("seven", "7")
               .Replace("eight", "8")
                .Replace("nine", "9")

let strToNum s = 
    match s with
    | "zero" -> 0
    |"one" -> 1
    |"two" -> 2
    | "three" -> 3
    | "four" -> 4
    | "five" -> 5
    | "six" -> 6
    | "seven" -> 7
    | "eight" -> 8
    | "nine" -> 9
    | _ -> failwith "not valid" 

let foo (s: string) = 
    let arg = 
        ["zero"; "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine"]
        |> List.map (fun d -> (d, s.IndexOf(d), s.LastIndexOf(d)))
    if List.isEmpty arg then s
    else
        let mutable res = s
        let mutable miniVal = -1
        let mins = 
            arg
            |> List.filter(fun (d, min, max) -> min <> -1)
        
        if not <| List.isEmpty mins then
            let (minVal, idx, _)  = 
                mins
                |> List.minBy (fun (a, b, c) -> b)
            miniVal <- idx
            res <- res.Replace(minVal, strToNum minVal |> string)
        let maxs = 
            arg
            |> List.filter(fun (d, min, max) -> max <> -1 && max > miniVal)
        if not <| List.isEmpty maxs then    
            let (maxVal, _, _) =
                maxs 
                |> List.maxBy (fun (a, b, c) -> c)
            res <- res.Replace(maxVal, strToNum maxVal |> string)
        res


let task2 = 
    readFile ()
    |> List.map foo
    |> List.map (fun s ->
        let digits = 
            s 
            |> Seq.filter(fun c -> Char.IsDigit(c)) 
            |> Seq.toList
        (int <| Char.ToString(List.head digits)) * 10 + (int <| Char.ToString(List.last digits))
        )
    |> List.sum
// printfn $"Task 1: {task1}"
printfn $"Task 2: {task2}"  // 54514 too low  // 54524 too high