#r "nuget: Expecto"

open System
open Expecto

let executeProcess (processName: string) (workingDirectory: string) (args: string list) =
    let psi = new Diagnostics.ProcessStartInfo(processName)
    args |> List.iter (fun arg -> psi.ArgumentList.Add(arg))
    psi.WorkingDirectory <- workingDirectory
    psi.UseShellExecute <- false
    psi.RedirectStandardOutput <- true
    psi.CreateNoWindow <- true        
    let proc = Diagnostics.Process.Start(psi) 
    let output = new Text.StringBuilder()
    proc.OutputDataReceived.Add(fun args -> output.Append(args.Data) |> ignore)
    proc.BeginOutputReadLine()
    proc.WaitForExit()
    proc.ExitCode, output.ToString()

let testDay day task1expected task2expected = 
    test $"Test day {day}" {
    let (exitCode, output) = executeProcess "dotnet" $"day{day}" ["fsi"; $"day{day}.fsx"]
    Expect.equal exitCode 0 "Program runs ok"
    let task1Idx = output.IndexOf("Task 1")
    let stringTrimmed = output.Substring(task1Idx).Replace("Task 1:", "").Replace("Task 2:", "").Trim()
    let taskResults = stringTrimmed.Split(" ")
    let task1 = taskResults[0]
    let task2 = taskResults[1]
    Expect.equal task1 task1expected "Task 1 has expected result"
    Expect.equal task2 task2expected "Task 2 has expected result"
  }

let tests =
    testList "Test all days" [
        testDay 1 "54331" "54518"
        testDay 2 "2268" "63542"
        testDay 3 "517021" "81296995"
        testDay 4 "20667" "5833065"
        testDay 5 "600279879" "20191102"

    ]
let exitcode = runTestsWithCLIArgs [] [||] tests
exit exitcode
