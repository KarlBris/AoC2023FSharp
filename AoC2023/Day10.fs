namespace AoC2023

open Utils
open System

module Day10 =

    type Direction =
        | U
        | D
        | L
        | R

    let opposite (dir: Direction) : Direction =
        match dir with
        | U -> D
        | D -> U
        | L -> R
        | R -> L

    let directionsSteppingInto (c: char) : Direction list =
        match c with
        | '|' -> [ U; D ]
        | '-' -> [ L; R ]
        | 'L' -> [ D; L ]
        | 'J' -> [ D; R ]
        | '7' -> [ U; R ]
        | 'F' -> [ U; L ]
        | 'S' -> [ U; D; L; R ]
        | _ -> []

    let directions =
        [ (U, (0, -1))
          (R, (1, 0))
          (D, (0, 1))
          (L, (-1, 0)) ]

    let positionsDirections (c: char) (pos: Position) : (Direction * Position) list =
        directions
        |> List.filter (fun (d, _) ->
            c
            |> directionsSteppingInto
            |> List.map opposite
            |> List.contains d)
        |> List.map (fun (d, p) -> (d, addPos pos p))

    let rec traverse
        (currentPositions: Position list)
        (step: int)
        (distanceMap: Map<Position, int>)
        (map: Map<Position, char>)
        : Map<Position, int> =
        let positionsToGoTo =
            currentPositions
            |> List.map (fun p -> p |> positionsDirections map[p])
            |> List.concat
            |> List.filter (fun (d, pos) ->
                (Map.containsKey pos map)
                && (not (Map.containsKey pos distanceMap))
                && (List.contains d (directionsSteppingInto (map[pos]))))
            |> List.distinctBy snd

        if List.length positionsToGoTo = 0 then
            distanceMap
        else
            let distanceMap' =
                positionsToGoTo
                |> List.fold (fun vMap (_, p) -> Map.add p step vMap) distanceMap

            traverse (positionsToGoTo |> List.map snd) (step + 1) distanceMap' map

    let findStartPos (loopMap: Map<Position, char>) : Position =
        loopMap
        |> Map.map (fun k v -> if v = 'S' then Some k else None)
        |> Map.values
        |> Seq.choose id
        |> Seq.head

    let findFarthestReaches (input: Map<Position, char>) : Map<Position, int> =
        let startPos = findStartPos input

        traverse [ startPos ] 1 ([| (startPos, 0) |] |> Map.ofArray) input

    let parseInput (input: string array) : Map<Position, char> =
        input
        |> Array.mapi (fun y cs ->
            cs
            |> Seq.mapi (fun x c -> ((x, y), c))
            |> Seq.toArray)
        |> Array.concat
        |> Map.ofArray

    let part1 (input: string) : string =
        input
        |> lines
        |> parseInput
        |> findFarthestReaches
        |> Map.values
        |> Seq.max
        |> string

    let padAndParseInput (input: string) : Map<Position, char> =
        let paddedLR =
            input
            |> lines
            |> Seq.map (fun l -> "." + l + ".")
            |> List.ofSeq

        let width = paddedLR |> Seq.head |> Seq.length

        let frame = (Seq.replicate width ".") |> String.Concat

        Seq.concat [ [ frame ]
                     paddedLR
                     [ frame ] ]
        |> Seq.toArray
        |> parseInput

    let leftRightOffsets (facingDirection: Direction) : (char * Position) array =
        match facingDirection with
        | U -> [| ('V', (-1, 0)); ('H', (1, 0)) |]
        | D -> [| ('V', (1, 0)); ('H', (-1, 0)) |]
        | L -> [| ('V', (0, 1)); ('H', (0, -1)) |]
        | R -> [| ('V', (0, -1)); ('H', (0, 1)) |]

    let forwardsPos (pos: Position) (facingDirection: Direction) : Position =
        directions
        |> List.find (fun (d, _) -> d = facingDirection)
        |> (fun (_, dPos) -> addPos pos dPos)

    let rec traverseLoop
        (currentPosition: Position)
        (facingDirection: Direction)
        (inOutMap: Map<Position, char>)
        (loopPositions: Position Set)
        (map: Map<Position, char>)
        : (Map<Position, char> * Position Set) =

        let loopPositions' = (Set.add currentPosition loopPositions)

        if map[currentPosition] = 'S'
           && Map.count inOutMap > 0 then
            (inOutMap, loopPositions')
        else
            // set values in inOutMap
            let inOutMap' =
                facingDirection
                |> leftRightOffsets
                |> Array.map (fun (hv, dPos) -> (hv, addPos currentPosition dPos))
                |> Array.fold (fun iom (hv, p) -> Map.add p hv iom) inOutMap

            // look if turn is needed
            let (newDirection, positionToGoTo) =
                currentPosition
                |> (fun p -> p |> positionsDirections map[p])
                |> List.filter (fun (d, pos) ->
                    (Map.containsKey pos map)
                    && (pos
                        <> (forwardsPos currentPosition (opposite facingDirection)))
                    && (List.contains d (directionsSteppingInto (map[pos]))))
                |> List.head

            // if turn is needed, apply turn and set values in inOutMap. no else
            let (inOutMap'', facingDirection') =
                if (newDirection <> facingDirection) then
                    (newDirection
                     |> leftRightOffsets
                     |> Array.map (fun (hv, dPos) -> (hv, addPos currentPosition dPos))
                     |> Array.fold (fun iom (hv, p) -> Map.add p hv iom) inOutMap',
                     newDirection)
                else
                    (inOutMap', facingDirection)

            // recursive call with new position
            traverseLoop positionToGoTo facingDirection' inOutMap'' loopPositions' map

    let rec findOutside (pos as (x, y): Position) (rlMap: Map<Position, char>) (cleanMap: Map<Position, char>) : char =
        match Map.tryFind pos rlMap with
        | Some c -> c
        | None -> findOutside (x, y + 1) rlMap cleanMap

    let crossPositions = directions |> List.map snd

    let rec floodIsland (poss: Position Set) (acc: Position Set) (cleanMap: Map<Position, char>) : Position Set =
        let newPositions =
            poss
            |> Set.map (fun pos ->
                crossPositions
                |> Set.ofList
                |> Set.map (fun dPos -> addPos pos dPos))
            |> Set.unionMany
            |> Set.filter (fun pos ->
                not (Set.contains pos acc)
                && match Map.tryFind pos cleanMap with
                   | None -> false
                   | Some c -> c = '.')

        if Set.isEmpty newPositions then
            acc
        else
            floodIsland newPositions (Set.union newPositions acc) cleanMap

    let rec makeIslands
        (potentialIslandPositions: Position Set)
        (islands: Position Set Set)
        (cleanMap: Map<Position, char>)
        : Position Set Set =
        if Set.isEmpty potentialIslandPositions then
            islands
        else
            let ps =
                potentialIslandPositions
                |> Seq.head
                |> Set.singleton

            let islandPositions = floodIsland ps ps cleanMap

            let restOfPositions = Set.difference potentialIslandPositions islandPositions

            makeIslands restOfPositions (Set.add islandPositions islands) cleanMap

    let floodInside (width: int) (rlMap: Map<Position, char>) (cleanMap: Map<Position, char>) : Position list list =
        let outsideChar = findOutside ((width / 2), 0) rlMap cleanMap

        let insideChar = if outsideChar = 'V' then 'H' else 'V'

        let insidePositions =
            rlMap
            |> Map.toArray
            |> Array.filter (fun (_, c) -> c = insideChar)
            |> Array.map fst
            |> Array.filter (fun pos -> cleanMap[pos] = '.')
            |> Set.ofArray

        let islands = makeIslands insidePositions Set.empty cleanMap

        islands |> Seq.toList |> List.map (Set.toList)

    let part2 (input: string) : string =
        let initialDirection = U

        let loopMap = input |> padAndParseInput

        let (rightLeftMap, loopPositions) =
            traverseLoop (findStartPos loopMap) initialDirection Map.empty Set.empty loopMap

        let width = input |> lines |> Array.head |> Seq.length

        loopMap
        |> Map.map (fun k v ->
            if Set.contains k loopPositions then
                v
            else
                '.')
        |> floodInside width rightLeftMap
        |> List.map (List.length)
        |> List.sum
        |> string
