﻿namespace AoC2023

open System

module Utils =

    type Direction = N|S|W|E

    type Position = (int * int)

    let addPos ((ax, ay): Position) ((bx, by): Position) : Position = (ax + bx, ay + by)

    let manhattanDistance ((a, b): Position) ((x, y): Position) : int = (abs (a - x)) + (abs (b - y))

    let makeLineBetween ((aX, aY): Position) ((bX, bY): Position) : Position list =
        if aX = bX then
            [for y in (min aY bY)..(max aY bY) -> (aX,y)]
        elif aY = bY then
            [for x in (min aX bX)..(max aX bX) -> (x,aY)]
        else
            failwith "not in line"

    let isValidPosition ((maxX, maxY): Position) (pos as (x, y): Position) : bool =
        x >= 0 && y >= 0 && x <= maxX && y <= maxY

    let directionOfChar (c: char) : Direction =
        match c with
        | 'U'
        | 'N' -> N
        | 'D'
        | 'S' -> S
        | 'L'
        | 'W' -> W
        | 'R'
        | 'E' -> E
        | _ -> failwith ""

    let directionReverse (dir:Direction) : Direction = 
        match dir with
        | N -> S
        | E -> W
        | S -> N
        | W -> E

    let oneStep (dir: Direction) ((x,y): Position) : Position = 
        match dir with
        | N -> (x,y-1)
        | E -> (x+1,y)
        | S -> (x,y+1)
        | W -> (x-1,y)

    let rec stepsInDirection (steps: int) (dir: Direction) (pos: Position) : Position list=
        if steps = 0 then
            [pos]
        else
            pos::(stepsInDirection (steps-1) dir (oneStep dir pos))

    let whichDirection (fromPos as (fX,fY): Position) (toPos as (tX,tY): Position) : Direction =
        if fX < tX && fY = tY then E
        elif fX > tX && fY = tY then W
        elif fX = tX && fY < tY then S
        elif fX = tX && fY > tY then N
        else failwith "nope"

    let manhattanNeighborPositions: (Position * Direction) array =
        [|((0,1), S);((0,-1), N);((1,0), E);((-1,0), W)|]

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
        |> Array.map stringTrim

    let semicolons (input: string) : string [] =
        input.Split([| "; "; ";" |], StringSplitOptions.RemoveEmptyEntries)
        
        |> Array.map stringTrim
    let colons (input: string) : string [] =
        input.Split([| ": "; ":" |], StringSplitOptions.RemoveEmptyEntries)
        |> Array.map stringTrim

    let hyphens (input: string) : string [] =
        input.Split([| "- "; "-" |], StringSplitOptions.RemoveEmptyEntries)
        |> Array.map stringTrim

    let slashes (input: string) : string [] =
        input.Split([| "/ "; "/" |], StringSplitOptions.RemoveEmptyEntries)
        |> Array.map stringTrim

    let split (splitString: string) (input: string) : string []=
        input.Split([| splitString |], StringSplitOptions.RemoveEmptyEntries)
        |> Array.map stringTrim
        
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
