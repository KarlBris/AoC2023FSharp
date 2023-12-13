namespace AoC2023

open Utils

module Day13 =

    let parseInput (input: string) : char array array array =
        input
        |> splitAtDoubleLines
        |> Array.map (lines >> Array.map (Array.ofSeq))

    let widenSelection (times: int) (pairIndex: int) (pattern: char array array) : (char array * char array) option =
        if (times >= pairIndex + 1)
           || (times >= pattern.Length - pairIndex - 1) then
            None
        else
            let patterns =
                pattern
                |> Array.removeManyAt (pairIndex - (times - 1)) (times * 2)
                |> Array.pairwise

            match patterns with
            | [||] -> None
            | ps -> Some ps[pairIndex - times]

    let rec checkDifference (a: char array) (b: char array) : int =
        if a.Length = 0 then
            0
        elif a[0] = b[0] then
            checkDifference (Array.tail a) (Array.tail b)
        else
            (checkDifference (Array.tail a) (Array.tail b))
            + 1

    let rec widenAndCheck
        (smudgeTolerance: int)
        (times: int)
        (pairIndex: int)
        (pattern: char array array)
        : (bool * int) =
        match widenSelection times pairIndex pattern with
        | None -> (true, smudgeTolerance)
        | Some (a, b) ->
            let diff = checkDifference a b

            if diff <= smudgeTolerance then
                widenAndCheck (smudgeTolerance - diff) (times + 1) pairIndex pattern
            else
                (false, smudgeTolerance)

    let checkReflection (smudgeTolerance: int) (pattern: char array array) : int option =
        [| 0 .. (pattern.Length - 2) |]
        |> Array.map (fun pairIndex -> (widenAndCheck smudgeTolerance 0 pairIndex pattern, pairIndex + 1))
        |> Array.filter (fun ((b, i), _) -> b && (i = 0))
        |> Array.map snd
        |> Array.tryHead

    let getPatternScore (part2: bool) (pattern: char array array) : int =
        let smudgeTolerance = if part2 then 1 else 0

        match checkReflection smudgeTolerance (pattern |> Array.transpose) with
        | Some n -> n
        | None ->
            match checkReflection smudgeTolerance pattern with
            | Some n -> n * 100
            | None -> failwith ""

    let doStuff (input: string) (part2: bool) : string =
        input
        |> parseInput
        |> Array.map (getPatternScore part2)
        |> Array.sum
        |> string

    let part1 (input: string) : string = doStuff input false

    let part2 (input: string) : string = doStuff input true
