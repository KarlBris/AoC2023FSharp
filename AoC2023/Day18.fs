namespace AoC2023

open Utils
open System

module Day18 =

    let parseInputStep (input: string) : (Direction * int) =
        input
        |> words
        |> (fun [| d; n; _ |] -> (directionOfChar (Seq.head d), int n))

    let parseInputStepHex (input: string) : (Direction * int) =
        let (hexString, dirChar) =
            input
            |> words
            |> Array.last
            |> Array.ofSeq
            |> (fun a -> (a[2..6], a[7]))

        let dir =
            match dirChar with
            | '0' -> E
            | '1' -> S
            | '2' -> W
            | '3' -> N
            | _ -> failwith ""

        let hexInt = Int32.Parse(hexString, System.Globalization.NumberStyles.HexNumber)

        (dir, hexInt)

    let rec makePositions
        (fromPos: Position)
        (acc: Position list)
        (instructions: (Direction * int) list)
        : Position list =
        match instructions with
        | [] -> acc
        | (dir, steps) :: is ->
            let endPos = nSteps dir steps fromPos
            makePositions endPos (fromPos :: acc) is

    let shoelace (positions: Position array) : int64 =
        let size = positions.Length
        let getYi (i: int) : int64 = positions[i] |> snd |> int64
        let getXi (i: int) : int64 = positions[i] |> fst |> int64

        let getYiPlusOne (i: int) : int64 =
            positions[eMod (i + 1) size] |> snd |> int64

        let getXiPlusOne (i: int) : int64 =
            positions[eMod (i + 1) size] |> fst |> int64

        let doubleRes =
            [ 0 .. (size - 1) ]
            |> List.map (fun i ->
                ((getYi i) + (getYiPlusOne i))
                * ((getXi i) - (getXiPlusOne i)))
            |> List.sum

        doubleRes / 2L

    let doStuff (parseFun: string -> Direction * int) (input: string) : string =
        let stepList =
            input
            |> lines
            |> Array.map parseFun
            |> List.ofArray

        let positions =
            stepList
            |> makePositions (0, 0) []
            |> Array.ofList
            |> Array.rev

        let stepSum = stepList |> List.map (snd >> int64) |> List.sum

        let area = shoelace positions

        (area + (stepSum / 2L) + 1L) |> string

    let part1 (input: string) : string = doStuff parseInputStep input

    let part2 (input: string) : string = doStuff parseInputStepHex input
