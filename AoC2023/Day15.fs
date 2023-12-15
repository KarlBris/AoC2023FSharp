namespace AoC2023

open Utils

module Day15 =

    type Entry = (char list * int)

    let parseInput (input: string) : char list array =
        input |> commas |> Array.map (Seq.toList)

    let parseInput2 (input: string) : (char list * char list) array =
        input
        |> commas
        |> Array.map (fun line ->
            let first =
                line
                |> Seq.takeWhile (fun c -> c <> '=' && c <> '-')
                |> List.ofSeq

            let second = line |> Seq.skip (first.Length) |> List.ofSeq
            (first, second))

    let rec hash (currentValue: int) (characters: char list) : int =
        match characters with
        | [] -> currentValue
        | c :: cs -> hash (((currentValue + (int c)) * 17) % 256) cs

    let part1 (input: string) : string =
        input
        |> parseInput
        |> Array.map (hash 0)
        |> Array.sum
        |> string

    let findAndRemoveFromMap (value: int) (label: char list) (boxMap: Map<int, Entry list>) : Map<int, Entry list> =
        boxMap
        |> Map.change value (fun op ->
            match op with
            | None -> None
            | Some x ->
                x
                |> List.filter (fun (cs, _) -> cs <> label)
                |> Some)

    let addToMap (value: int) (label: char list) (number: int) (boxMap: Map<int, Entry list>) : Map<int, Entry list> =
        boxMap
        |> Map.change value (fun op ->
            match op with
            | None -> Some [ (label, number) ]
            | Some x ->
                match x |> List.tryFindIndex (fun (cs, _) -> cs = label) with
                | Some i -> Some(x |> List.updateAt i (label, number))
                | None -> Some(List.append x [ (label, number) ]))

    let part2 (input: string) : string =
        input
        |> parseInput2
        |> Array.fold
            (fun state (f, s) ->
                let value = hash 0 f

                match s with
                | '=' :: n :: _ -> addToMap value f (charToInt n) state
                | '-' :: _ -> findAndRemoveFromMap value f state
                | _ -> failwith "")
            Map.empty
        |> Map.map (fun _ t -> t |> List.mapi (fun i v -> (i, v)))
        |> Map.toArray
        |> Array.map (fun (box, ps) ->
            ps
            |> List.map (fun (slot, (_, focalLength)) ->
                let ans = (box + 1) * (slot + 1) * focalLength
                ans)
            |> List.sum)
        |> Array.sum
        |> string
