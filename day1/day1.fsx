open System.IO
open System

let readFile () = 
    File.ReadLines "input.txt"
    |> Seq.toList

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

let replaceStringDigits (s: String) =
    s
        .Replace("one", "1")
        .Replace("two", "2")
        .Replace("three", "3")
        .Replace("four", "4")
        .Replace("five", "5")
        .Replace("six", "6")
        .Replace("seven", "7")
        .Replace("eight", "8")
        .Replace("nine", "9")

let findDigitsBruteForce (s: string) =
    let fs =
        s.Replace("twone", "2").Replace("oneight", "1").Replace("eightwo", "8") |> replaceStringDigits
    let first = fs |> Seq.find Char.IsDigit |> Char.ToString |> int
    let ls =
        s.Replace("twone", "1").Replace("oneight", "8").Replace("eightwo", "2") |> replaceStringDigits
    let last =
        ls |> Seq.rev |> Seq.find Char.IsDigit |> Char.ToString |> int
    first * 10 + last

let task2 = 
    readFile ()
    |> List.map findDigitsBruteForce
    |> List.sum
    
printfn $"Task 1: {task1}" // 54331
printfn $"Task 2: {task2}" // 54518