namespace AoC2023

open Utils

module Day09 =

    let parseInput (input: string) : int array array =
        input
        |> lines
        |> Array.map (words >> Array.map (int))

    let rec pyramidize (numbers: (int array) list) : (int array) list =
        let latestNumbers = numbers |> List.head

        if Array.forall ((=) 0) latestNumbers then
            numbers
        else
            let diffs =
                latestNumbers
                |> Array.pairwise
                |> Array.map (fun (a, b) -> b - a)

            pyramidize (diffs :: numbers)

    let rec fillNewNumbers (op: int -> int -> int) (prevNum: int) (numbers: int list) : int =
        match numbers with
        | [] -> prevNum
        | n :: ns -> fillNewNumbers op (op n prevNum) ns

    let extrapolateSequence (backwards: bool) (numbers: int array) : int =
        let (getElement, op) =
            if backwards then
                (Array.head, (-))
            else
                (Array.last, (+))

        numbers
        |> List.singleton
        |> pyramidize
        |> List.map (getElement)
        |> fillNewNumbers op 0

    let doStuff (backwards: bool) (input: string) : string =
        input
        |> parseInput
        |> Array.map (extrapolateSequence backwards)
        |> Array.sum
        |> string

    let part1 (input: string) : string = input |> doStuff false

    let part2 (input: string) : string = input |> doStuff true
