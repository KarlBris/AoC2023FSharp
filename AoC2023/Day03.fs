namespace AoC2023

open Utils
open System

module Day03 =
    type Position = (int * int)

    let numbers = [ '0' .. '9' ]
    let nonSymbols = '.' :: numbers

    let findPositionsBy (someFun: char -> bool) (input: (char seq) array) : Position list =
        input
        |> Array.mapi (fun y row ->
            row
            |> Seq.mapi (fun x c -> if (someFun c) then Some(x, y) else None))
        |> Seq.concat
        |> Seq.choose id
        |> Seq.toList

    let adjacentPositions (width: int) (height: int) ((x, y): Position) : Position list =
        [ for dx in [ -1; 0; 1 ] do
              for dy in [ -1; 0; 1 ] -> (x + dx, y + dy) ]
        |> List.filter (fun (x', y') -> x' >= 0 && x' < width && y' >= 0 && y' < height)

    let findNumbersAdjacentToSymbol
        (position: Position)
        (input: (char seq) array)
        (width: int)
        (height: int)
        : Position list =
        position
        |> adjacentPositions width height
        |> List.map (fun (x, y) -> ((x, y), Seq.item x (input[y])))
        |> List.map (fun (pos, c) ->
            if List.contains c numbers then
                Some pos
            else
                None)
        |> List.choose id

    let rec stepLeftToFirstNumberPos (row: (char array)) (pos: int) : int =
        if List.contains row[pos] numbers then
            if pos = 0 then
                0
            else
                stepLeftToFirstNumberPos row (pos - 1)
        else
            pos + 1

    let expandNumber ((x, y): Position) (input: (char seq) array) : (Position * int) =
        let row = input[y] |> Seq.toArray
        let firstPos = stepLeftToFirstNumberPos row x

        let number =
            row[firstPos..]
            |> Array.takeWhile (fun c -> List.contains c numbers)
            |> String.Concat
            |> int

        ((firstPos, y), number)

    let part1 (input: string) : string =
        let parsed: (char seq) array = input |> lines |> Array.map (Seq.cast)
        let height = Array.length parsed
        let width = Seq.length parsed[0]

        parsed
        |> findPositionsBy (fun c -> not (List.contains c nonSymbols))
        |> List.map (fun sPos -> findNumbersAdjacentToSymbol sPos parsed width height)
        |> List.concat
        |> List.distinct
        |> List.map (fun p -> expandNumber p parsed)
        |> List.distinctBy (fun (p, _) -> p)
        |> List.map snd
        |> List.sum
        |> string

    let part2 (input: string) : string =
        let parsed: (char seq) array = input |> lines |> Array.map (Seq.cast)
        let height = Array.length parsed
        let width = Seq.length parsed[0]

        parsed
        |> findPositionsBy (fun c -> c = '*')
        |> List.map (fun sPos -> findNumbersAdjacentToSymbol sPos parsed width height)
        |> List.map (fun posList ->
            posList
            |> List.map (fun p -> expandNumber p parsed)
            |> List.distinctBy (fun (p, _) -> p)
            |> List.map snd)
        |> List.filter (fun l -> List.length l = 2)
        |> List.map (List.fold (*) 1)
        |> List.sum
        |> string
