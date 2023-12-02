namespace AoC2023

open Utils

module Day02 =

    let parseLine (input: string) : (int * (int * string) array array) =
        input
        |> (colons >> twoArrayToTuple)
        |> (fun (id, rest) -> (int (id.Substring(5)), semicolons rest))
        |> (fun (id, lists) ->
            (id,
             lists
             |> Array.map commas
             |> Array.map (fun ts ->
                 ts
                 |> Array.map words
                 |> Array.map (twoArrayToTuple >> (fun (a, b) -> (int a, b))))))

    let maxRed = 12
    let maxGreen = 13
    let maxBlue = 14

    let isDealImpossible ((x, col): int * string) : bool =
        match col with
        | "red" -> x > maxRed
        | "green" -> x > maxGreen
        | _ -> x > maxBlue

    let isHandImpossible (gs: (int * string) array) : bool = Array.exists isDealImpossible gs

    let isGamePossible ((_, game): int * (int * string) array array) : bool =
        game |> Array.exists isHandImpossible |> not

    let maxGameColors (game: (int * string) array array) : int array =
        game
        |> Array.concat
        |> Array.groupBy snd
        |> Array.map snd
        |> Array.map (Array.maxBy (fun (i, _) -> i))
        |> Array.map fst

    let gamePower ((_, game): int * (int * string) array array) : int =
        game
        |> maxGameColors
        |> Array.fold (fun s i -> s * i) 1

    let part1 (input: string) : string =
        input
        |> lines
        |> Array.map parseLine
        |> Array.filter isGamePossible
        |> Array.map fst
        |> Array.sum
        |> string

    let part2 (input: string) : string =
        input
        |> lines
        |> Array.map parseLine
        |> Array.map gamePower
        |> Array.sum
        |> string
