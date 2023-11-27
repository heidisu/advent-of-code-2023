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
        testDay 1 "66306" "195292"
        testDay 2 "12855" "13726"
        testDay 3 "7428" "2650"
        testDay 4 "567" "907"
        testDay 5 "MQTPGLLDN" "LVZPSTTCZ"
        testDay 6 "1300" "3986"
        testDay 7 "1749646" "1498966"
        testDay 8 "1870" "517440"
        testDay 9 "6067" "2471"
        testDay 10 "14920" "BUCACBUZ"
        testDay 13 "6420" "22000"
        testDay 14 "1016" "25402"
        testDay 18 "4604" "2604"
    ]
let exitcode = runTestsWithCLIArgs [] [||] tests
exit exitcode
