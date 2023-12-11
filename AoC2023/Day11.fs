namespace AoC2023

open Utils

module Day11 =

    let parseInput (input: string) : (Position array * (int array * int array)) =
        let positions =
            input
            |> lines
            |> Array.mapi (fun y cs ->
                cs
                |> Seq.mapi (fun x c -> if c = '#' then Some(x, y) else None))
            |> Seq.concat
            |> Seq.choose id
            |> Array.ofSeq

        let emptyLines =
            input
            |> lines
            |> Array.mapi (fun line cs ->
                if cs |> Seq.forall (fun c -> c = '.') then
                    Some line
                else
                    None)
            |> Array.choose id

        let emptyColumns =
            input
            |> lines
            |> Array.map (Array.ofSeq)
            |> Array.transpose
            |> Array.mapi (fun line cs ->
                if cs |> Seq.forall (fun c -> c = '.') then
                    Some line
                else
                    None)
            |> Array.choose id

        (positions, (emptyLines, emptyColumns))

    let rec makePairs (list: 'T list) : (('T * 'T) list list) =
        match list with
        | [] -> []
        | e :: es ->
            (es |> List.map (fun e2 -> (e, e2)))
            :: (makePairs es)

    let crossed (a: int) (b: int) (empty: int array) : int64 =
        empty
        |> Array.filter (fun l -> l > (min a b) && l < (max a b))
        |> Array.length
        |> int64

    let findDistance
        (multiplier: int)
        (emptyLines: int array)
        (emptyColumns: int array)
        ((p1, p2): (Position * Position))
        : int64 =
        let plainDistance = manhattanDistance p1 p2 |> int64

        let multiplier = ((int64 multiplier) - 1L)

        let emtpyLinesCrossed = crossed (snd p1) (snd p2) emptyLines
        let emptyColumnsCrossed = crossed (fst p1) (fst p2) emptyColumns

        plainDistance
        + (emtpyLinesCrossed * multiplier)
        + (emptyColumnsCrossed * multiplier)

    let sumDistances (expansionMultiplier: int) (input: string) : string =
        let (positions, (emptyLines, emptyColumns)) = parseInput input

        let allPositionPairs =
            positions
            |> List.ofArray
            |> makePairs
            |> List.concat

        allPositionPairs
        |> List.map (findDistance expansionMultiplier (emptyLines) (emptyColumns))
        |> List.sum
        |> string

    let part1 (input: string) : string = input |> sumDistances 2

    let part2 (input: string) : string = input |> sumDistances 1000000
