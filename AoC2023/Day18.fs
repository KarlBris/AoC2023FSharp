namespace AoC2023

open Utils
open System

module Day18 =

    let parseInputStep (input: string) : (Direction * int * string) =
        input
        |> words
        |> (fun [| d; n; s |] -> (directionOfChar (Seq.head d), int n, s))

    let rec digTrench
        (pos: Position)
        (acc: Map<Position, string>)
        (instructions: (Direction * int * string) list)
        : Map<Position, string> =
        match instructions with
        | [] -> acc
        | (dir, n, str) :: is ->

            let steps = pos |> stepsInDirection n dir |> List.tail

            let newAcc =
                steps
                |> List.fold (fun a p -> a |> Map.add p str) acc

            let lastStep = List.last steps

            digTrench lastStep newAcc is

    let rec floodFill (poss: Position Set) (acc: Position Set) (cleanMap: Map<Position, string>) : Position Set =
        let newPositions =
            poss
            |> Set.map (fun pos ->
                manhattanNeighborPositions
                |> Array.map fst
                |> Set.ofArray
                |> Set.map (fun dPos -> addPos pos dPos))
            |> Set.unionMany
            |> Set.filter (fun pos ->
                not (Set.contains pos acc)
                && not (Map.containsKey pos cleanMap))

        if Set.isEmpty newPositions then
            acc
        else
            floodFill newPositions (Set.union newPositions acc) cleanMap

    let part1 (input: string) : string =
        let hej =
            input
            |> lines
            |> Array.map parseInputStep
            |> List.ofArray

        let trench = digTrench (0, 0) Map.empty hej

        let floodedAcc = floodFill (Set.singleton (1, 1)) Set.empty trench


        string (trench.Count + floodedAcc.Count)

    let part2 (input: string) : string =

        // TODO: flood fill is not feasible. Change to some sort of line scanning method? 
        // TOOD: Acually making all the trenches is also too costly. Figure something out there
        input
