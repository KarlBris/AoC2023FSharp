namespace AoC2023

open System.IO
open System.Diagnostics

module TestRunner =

    let getInput day =
        let filename = $"..\..\..\inputs\input_day{day}.txt"

        if File.Exists filename then
            filename
            |> File.ReadAllText
            |> String.filter (fun c -> c <> '\r')
        else
            failwith $"Input file {filename} not found"

    let makeComparison (expectedResults: string []) (results: string []) =
        Array.zip results expectedResults
        |> Array.map (fun (r, e) -> (r, e, r = e))

    let printStatus ((res, expectedRes, success): string * string * bool) =
        printfn "%s! Got %s, expected %s." (if success then "Success" else "Failure") res expectedRes

    let run (examples: string []) expectedResults realInput (func: string -> string) title =
        printfn title

        if examples.Length = 0 then
            printfn "No examples found, running the real input..."
        else
            printfn "Running and verifying examples before the real input..."

        let resultList =
            examples
            |> Array.map func
            |> makeComparison expectedResults

        resultList |> Array.map printStatus |> ignore

        let examplesSuccessful =
            resultList
            |> Array.fold (fun b1 (_, _, b2) -> b1 && b2) true

        if examplesSuccessful then
            printfn "All examples were successful, running the real input..."
            let timer = new Stopwatch()
            timer.Start()
            printfn "Result from real input: %s" (func realInput)
            timer.Stop()
            printfn "Time elapsed: %A" timer.Elapsed
        else
            printfn "Some examples were not successful. PLEASE DO BETTER"

        printfn ""

    // Day1
    let input1 = getInput 1

    let examples1_1 =
        [| "1abc2\npqr3stu8vwx\na1b2c3d4e5f\ntreb7uchet" |]

    let exampleResults1_1 = [| "142" |]

    let examples1_2 =
        [| "two1nine\neightwothree\nabcone2threexyz\nxtwone3four\n4nineeightseven2\nzoneight234\n7pqrstsixteen" |]

    let exampleResults1_2 = [| "281" |]