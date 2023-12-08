namespace AoC2023

open Utils

module Day08 =

    let parseLine (input: string) : (string * (string * string)) =
        (input.Substring(0, 3), (input.Substring(7, 3), input.Substring(12, 3)))

    let parseInput (input: string) : char array * (string * (string * string)) array =

        let (steps, map) = input |> splitAtDoubleLines |> twoArrayToTuple

        (steps |> Array.ofSeq, map |> lines |> Array.map parseLine)

    let rec takeStep (stepsList: char array) (routeMap: Map<string,(string*string)>) (poss: string array) (stepsTaken: int64) : int64 =
        //if poss |> Array.exists (fun pos -> pos.Substring(2) = "Z") then
        if stepsTaken % (12361L) = 0L then
            //poss |> Array.iteri (fun i p -> if p.Substring(2) = "Z" then printfn $"{i}@{stepsTaken}: {p}" else ())
            let matches = poss |> Array.filter (fun p -> p.Substring(2) = "Z") |> Array.length
            if matches > 2 then
                poss |> Array.iteri (fun i p -> if p.Substring(2) = "Z" then printf $"{i}" else ())
                printfn ""


        if poss |> Array.forall (fun pos -> pos.Substring(2) = "Z") then 
            stepsTaken
        else
            match stepsList[(stepsTaken % (int64 stepsList.Length)) |> int] with
            | 'R' -> takeStep stepsList routeMap (poss |> Array.map (fun pos -> snd routeMap[pos])) (stepsTaken + 1L)
            | _ ->   takeStep stepsList routeMap (poss |> Array.map (fun pos -> fst routeMap[pos])) (stepsTaken + 1L)


    let stepToZZZ ((steps, routes): char array * (string * (string * string)) array) : int64 =
        let routeMap = Map.ofArray routes
        takeStep steps routeMap [|"AAA"|] 0L


    let multiStepToZZZ ((steps, routes): char array * (string * (string * string)) array) : int64 =
        let routeMap = Map.ofArray routes

        let positions = 
            routeMap
            |> Map.keys
            |> Seq.filter (fun k -> k.Substring(2,1) = "A")
            |> Seq.toArray
        
        takeStep steps routeMap positions 0L


    let part1 (input: string) : string =
        let hej = input |> parseInput

        hej
        |> stepToZZZ
        |> string

    let part2 (input: string) : string =
        let hej = input |> parseInput

        hej
        |> multiStepToZZZ
        |> string