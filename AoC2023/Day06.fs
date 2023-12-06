namespace AoC2023

open Utils
open System

module Day06 =

    let parseInput (input: string) : (int64 array * int64 array) =
        input
        |> lines
        |> Array.map colons
        |> Array.map Array.tail
        |> Array.concat
        |> Array.map (words >> (Array.map int64))
        |> twoArrayToTuple

    let parse2 (input: string) : (int64 * int64) =
        input
        |> lines
        |> Array.map (
            (Seq.filter (Char.IsDigit))
            >> String.Concat
            >> int64
        )
        |> twoArrayToTuple

    let rec findLower ((t, d): (int64 * int64)) (ht: int64) =
        if (t - ht) * ht > d then
            ht
        else
            findLower (t, d) (ht + 1L)

    let rec findUpper ((t, d): (int64 * int64)) (ht: int64) =
        if (t - ht) * ht > d then
            ht
        else
            findUpper (t, d) (ht - 1L)

    let findMargin ((t, d): (int64 * int64)) : int =
        let lowerBound = (findLower (t, d) 0L) |> int
        let upperBound = (findUpper (t, d) t) |> int

        upperBound - lowerBound + 1

    let part1 (input: string) : string =
        let (ts, ds) = parseInput input

        Array.zip ts ds
        |> Array.map findMargin
        |> Array.fold (*) 1
        |> string

    let part2 (input: string) : string = input |> parse2 |> findMargin |> string
