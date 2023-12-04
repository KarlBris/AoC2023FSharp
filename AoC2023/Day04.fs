namespace AoC2023

open Utils
open System

module Day04 =
    
    type Card = (int * (int array * int array))

    let parseLine (input: string) : Card =
        let pair = 
            input
            |> colons
            |> twoArrayToTuple

        let cardNum = 
            pair
            |> fst
            |> words
            |> (fun x -> x[1])
            |> int

        pair
        |> snd
        |> (fun s -> s.Split([| "|" |], StringSplitOptions.RemoveEmptyEntries))
        |> Array.map words
        |> Array.map (Array.map int)
        |> (fun a -> (cardNum, (twoArrayToTuple a)))

    let countWins1 ((_,(winning, my)):(int * (int array * int array))) : int =
        winning
        |> Array.fold (fun s num -> if Array.contains num my then s+1 else s) 0
        |> (fun i -> if i = 0 then 0 else pown 2 (i-1))

    let countWins2 ((winning, my):(int array * int array)) : int =
        winning
        |> Array.fold (fun s num -> if Array.contains num my then s+1 else s) 0

    let part1 (input: string) : string =
        input
        |> lines
        |> Array.map parseLine
        |> Array.map countWins1
        |> Array.sum
        |> string

    let rec runScratches (countMap : Map<int,int>) (currentCardNumber: int) (cards: Map<int,(int array * int array)>) : Map<int,int> =
        match Map.tryFind currentCardNumber cards with
        | None -> countMap
        | Some currentCard -> 
            let wins = countWins2 currentCard
            let addList = [(currentCardNumber+1)..(currentCardNumber+wins)]
            let countMap' = 
                addList
                |> List.fold (fun m add -> 
                    Map.change add (fun x -> 
                        match x with
                        | None -> None
                        | Some x2 -> Some (x2 + countMap[currentCardNumber])) m) countMap

            runScratches countMap' (currentCardNumber + 1) cards

    let part2 (input: string) : string =
        let parsed =
            input
            |> lines
            |> Array.map parseLine

        let countMap = 
            parsed
            |> Array.map (fun (n,_) -> (n,1))
            |> Map.ofArray

        parsed
        |> Map.ofArray
        |> runScratches countMap 1 
        |> Map.values
        |> Seq.sum
        |> string
