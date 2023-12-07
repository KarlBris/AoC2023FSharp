namespace AoC2023

open Utils

module Day07 =

    let strength = "AKQJT98765432"
    let strengthJ = "AKQT98765432J"

    let strengths =
        [| 1..13 |]
        |> Array.zip (strength |> Seq.toArray |> Array.rev)
        |> Map.ofArray

    let strengthsJoker =
        [| 1..13 |]
        |> Array.zip (strengthJ |> Seq.toArray |> Array.rev)
        |> Map.ofArray

    let categorizeHand (hand: char array) : int =
        let handGroups = Array.groupBy id hand |> Array.map snd

        if Array.length handGroups = 1 then // Five of a kind
            7
        elif Array.exists (fun a -> Array.length a = 4) handGroups then // Four of a kind
            6
        elif (Array.length handGroups[0] = 3
              && Array.length handGroups[1] = 2)
             || (Array.length handGroups[0] = 2
                 && Array.length handGroups[1] = 3) then // Full house
            5
        elif Array.exists (fun a -> Array.length a = 3) handGroups then // three of a kind
            4
        elif (handGroups
              |> Array.filter (fun a -> Array.length a = 2)
              |> Array.length) = 2 then // Two pair
            3
        elif (handGroups
              |> Array.filter (fun a -> Array.length a = 2)
              |> Array.length) = 1 then // One pair
            2
        else // High card
            1

    let handValue (hand: char array) : int array =
        let cardValue = hand |> Array.map (fun c -> strengths[c])

        let categoryValue = [| (categorizeHand hand) |]

        Array.append categoryValue cardValue

    let handValueJ (hand: char array) (originalHand: char array) : int array =
        let cardValue =
            originalHand
            |> Array.map (fun c -> strengthsJoker[c])

        let categoryValue = [| (categorizeHand hand) |]

        Array.append categoryValue cardValue

    let parseLine (input: string) : (char array * int) =
        input
        |> words
        |> twoArrayToTuple
        |> (fun (a, b) -> (Seq.toArray a, int b))

    let part1 (input: string) : string =
        input
        |> lines
        |> Array.map parseLine
        |> Array.map (fun (c, b) -> (handValue c, b))
        |> Array.sortBy fst
        |> Array.mapi (fun i (_, b) -> (i + 1) * b)
        |> Array.sum
        |> string

    let cardsWithoutJoker = "AKQT98765432" |> Seq.toArray

    let elaborateHandAtPosition (hand: char list) (i: int) : (char list) list =
        [ for nc in cardsWithoutJoker -> List.mapi (fun ix v -> if ix = i then nc else v) hand ]

    let possibleJokerHands (hand: char list) : (char list) list =
        [ 0..4 ]
        |> List.fold
            (fun s i ->
                s
                |> List.map (fun h ->
                    if List.item i h = 'J' then
                        let x = elaborateHandAtPosition h i
                        x
                    else
                        [ h ])
                |> List.concat)
            [ hand ]

    let maxHandValue (hand: char array) : int array =
        hand
        |> List.ofArray
        |> possibleJokerHands
        |> List.map (fun e -> handValueJ (List.toArray e) hand)
        |> List.sortDescending
        |> List.head

    let part2 (input: string) : string =
        input
        |> lines
        |> Array.map parseLine
        |> Array.map (fun (c, b) -> (maxHandValue c, b))
        |> Array.sortBy fst
        |> Array.mapi (fun i (_, b) -> (i + 1) * b)
        |> Array.sum
        |> string
