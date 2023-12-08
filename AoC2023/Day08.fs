namespace AoC2023

open Utils

module Day08 =

    let parseLine (input: string) : (string * (string * string)) =
        (input.Substring(0, 3), (input.Substring(7, 3), input.Substring(12, 3)))

    let parseInput (input: string) : char array * (string * (string * string)) array =
        let (steps, map) = input |> splitAtDoubleLines |> twoArrayToTuple
        (steps |> Array.ofSeq, map |> lines |> Array.map parseLine)

    let rec takeStep
        (stepsList: char array)
        (routeMap: Map<string, (string * string)>)
        (pos: string)
        (stepsTaken: int)
        : int =
        if pos.Substring(2) = "Z" then
            stepsTaken
        else
            match stepsList[stepsTaken % (stepsList.Length)] with
            | 'R' -> takeStep stepsList routeMap (snd routeMap[pos]) (stepsTaken + 1)
            | _ -> takeStep stepsList routeMap (fst routeMap[pos]) (stepsTaken + 1)

    let stepToZZZ ((steps, routes): char array * (string * (string * string)) array) : int =
        let routeMap = Map.ofArray routes
        takeStep steps routeMap "AAA" 0

    let multiStepToZZZ ((steps, routes): char array * (string * (string * string)) array) : int64 =
        let routeMap = Map.ofArray routes

        let cycleLengths =
            routeMap
            |> Map.keys
            |> Seq.filter (fun k -> k.Substring(2, 1) = "A")
            |> Seq.toArray
            |> Array.map (fun p -> takeStep steps routeMap p 0)

        let gcd =
            cycleLengths
            |> Array.map (divisors)
            |> Array.map Set.ofArray
            |> Set.ofArray
            |> Set.intersectMany
            |> Set.maxElement
            |> int64

        let dividedCycles =
            cycleLengths
            |> Array.map (fun n -> ((int64 n) / gcd))

        gcd * (dividedCycles |> Array.fold (*) 1L)

    let part1 (input: string) : string =
        input |> parseInput |> stepToZZZ |> string

    let part2 (input: string) : string =
        input |> parseInput |> multiStepToZZZ |> string
