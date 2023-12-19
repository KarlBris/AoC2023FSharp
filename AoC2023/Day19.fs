namespace AoC2023

open Utils
open System
open System.Text.RegularExpressions

module Day19 =

    type Workflow = (string * ((((char * char * int) option) * string) list))

    let parseWorkflow (input: string) : Workflow =
        let regex = Regex("(.+){(.*)}", RegexOptions.Compiled)
        let gs = regex.Match(input).Groups

        let name = gs[1].Value

        let rules = gs[2].Value

        let parsedRules =
            rules
            |> commas
            |> List.ofArray
            |> List.map (fun singleRule ->
                let a = singleRule |> colons

                if a.Length = 1 then
                    (None, a[0])
                else
                    let (condition, destination) = twoArrayToTuple a
                    let category = condition[0]
                    let comparison = condition[1]
                    let value = condition[2..] |> int

                    (Some(category, comparison, value), destination))

        (name, parsedRules)

    let parsePart (input: string) : (int * int * int * int) = 
        let length = input.Length
        
        let vs =
            input.Substring(1, length-2)
            |> commas
            |> Array.map (fun s -> s.Substring(2) |> int)

        (vs[0],vs[1],vs[2],vs[3])

    let part1 (input: string) : string =
        let (workflowInputs, partInputs) = input |> splitAtDoubleLines |> twoArrayToTuple

        let parsedWorkflows = workflowInputs |> lines |> Array.map parseWorkflow

        let parsedParts = partInputs |> lines |> Array.map parsePart

        input

    let part2 (input: string) : string = input
