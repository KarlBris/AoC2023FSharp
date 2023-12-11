﻿namespace AoC2023

open System

module Utils =

    type Position = (int * int)

    let addPos ((ax, ay): Position) ((bx, by): Position) : Position = (ax + bx, ay + by)

    let manhattanDistance ((a, b): Position) ((x, y): Position) : int = (abs (a - x)) + (abs (b - y))

    let lines (input: string) : string [] =
        input.Split([| "\r\n"; "\n"; "\r" |], StringSplitOptions.RemoveEmptyEntries)

    let splitAtDoubleLines (input: string) : string [] =
        input.Split([| "\r\n\r\n"; "\n\n"; "\r\r" |], StringSplitOptions.RemoveEmptyEntries)

    let stringTrim (string: string) : string = string.Trim()

    let words (input: string) : string [] =
        input.Split([| " "; "\t" |], StringSplitOptions.RemoveEmptyEntries)
        |> Array.map stringTrim

    let commas (input: string) : string [] =
        input.Split([| ", "; "," |], StringSplitOptions.RemoveEmptyEntries)

    let semicolons (input: string) : string [] =
        input.Split([| "; "; ";" |], StringSplitOptions.RemoveEmptyEntries)

    let colons (input: string) : string [] =
        input.Split([| ": "; ":" |], StringSplitOptions.RemoveEmptyEntries)

    let hyphens (input: string) : string [] =
        input.Split([| "- "; "-" |], StringSplitOptions.RemoveEmptyEntries)

    let slashes (input: string) : string [] =
        input.Split([| "/ "; "/" |], StringSplitOptions.RemoveEmptyEntries)

    let isAllUppercase (input: string) : bool =
        input |> Seq.forall (fun c -> Char.IsUpper c)

    let twoArrayToTuple<'T> (arrayWithTwoElements: 'T []) : ('T * 'T) =
        match arrayWithTwoElements with
        | [| a; b |] -> (a, b)
        | _ -> failwithf "Array does not contain exactly two elements! %A" arrayWithTwoElements

    let eMod (a: int) (b: int) : int = ((a % b) + b) % b

    let eMod64 (a: int64) (b: int64) : int64 = ((a % b) + b) % b

    let charToInt (c: char) : int = int c - int '0'

    let divisors (n: int) : int array =
        [| 1..n |]
        |> Array.map (fun x -> (x, n % x))
        |> Array.filter (fun (_, n) -> n = 0)
        |> Array.map fst

    // From http://www.fssnip.net/4u/title/Very-Fast-Permutations
    let rec permutations =
        function
        | [] -> seq [ List.empty ]
        | x :: xs -> Seq.collect (insertions x) (permutations xs)

    and insertions x =
        function
        | [] -> [ [ x ] ]
        | (y :: ys) as xs ->
            (x :: xs)
            :: (List.map (fun x -> y :: x) (insertions x ys))
