namespace AoC2023

open Utils
open System

module Day01 =

    let isDigit (c: char) : bool = [ '0' .. '9' ] |> List.contains c

    let findFirstAndLastDigits (input: char seq) : (char * char) =
        input
        |> Seq.filter isDigit
        |> (fun cs -> (Seq.head cs, Seq.last cs))

    let rec replaceDigitsFromFront (input: char list) : char list =
        match input with
        | 'o' :: 'n' :: 'e' :: cs -> '1' :: cs
        | 't' :: 'w' :: 'o' :: cs -> '2' :: cs
        | 't' :: 'h' :: 'r' :: 'e' :: 'e' :: cs -> '3' :: cs
        | 'f' :: 'o' :: 'u' :: 'r' :: cs -> '4' :: cs
        | 'f' :: 'i' :: 'v' :: 'e' :: cs -> '5' :: cs
        | 's' :: 'i' :: 'x' :: cs -> '6' :: cs
        | 's' :: 'e' :: 'v' :: 'e' :: 'n' :: cs -> '7' :: cs
        | 'e' :: 'i' :: 'g' :: 'h' :: 't' :: cs -> '8' :: cs
        | 'n' :: 'i' :: 'n' :: 'e' :: cs -> '9' :: cs
        | c :: cs -> c :: (replaceDigitsFromFront (cs))
        | [] -> []

    let rec replaceDigitsFromBack (input: char list) : char list =
        match (Seq.toList input) with
        | 'e' :: 'n' :: 'o' :: cs -> '1' :: cs
        | 'o' :: 'w' :: 't' :: cs -> '2' :: cs
        | 'e' :: 'e' :: 'r' :: 'h' :: 't' :: cs -> '3' :: cs
        | 'r' :: 'u' :: 'o' :: 'f' :: cs -> '4' :: cs
        | 'e' :: 'v' :: 'i' :: 'f' :: cs -> '5' :: cs
        | 'x' :: 'i' :: 's' :: cs -> '6' :: cs
        | 'n' :: 'e' :: 'v' :: 'e' :: 's' :: cs -> '7' :: cs
        | 't' :: 'h' :: 'g' :: 'i' :: 'e' :: cs -> '8' :: cs
        | 'e' :: 'n' :: 'i' :: 'n' :: cs -> '9' :: cs
        | c :: cs -> c :: (replaceDigitsFromBack cs)
        | [] -> []

    let findFirstDigits (input: string) : char =
        input
        |> Seq.toList
        |> replaceDigitsFromFront
        |> Seq.filter isDigit
        |> Seq.head

    let findLastDigits (input: string) : char =
        input
        |> Seq.rev
        |> String.Concat
        |> Seq.toList
        |> replaceDigitsFromBack
        |> Seq.rev
        |> Seq.filter isDigit
        |> Seq.last

    let part1 (input: string) : string =
        input
        |> lines
        |> Array.map findFirstAndLastDigits
        |> Array.map (fun (a, b) -> (String.Concat [ a; b ]))
        |> Array.map int
        |> Array.sum
        |> string

    let part2 (input: string) : string =
        let inputArray = lines input
        let firsts = inputArray |> Array.map findFirstDigits
        let lasts = inputArray |> Array.map findLastDigits
        let combined = Array.zip firsts lasts

        combined
        |> Array.map (fun (a, b) -> (String.Concat [ a; b ]))
        |> Array.map int
        |> Array.sum
        |> string
