namespace AoC2023

open Utils

module Day14 =

    type Direction =
        | N
        | W
        | S
        | E

    let parseInput (input: string) : (Position Set) * (Map<Position, char>) * int =
        let grid = input |> lines |> Array.map Array.ofSeq

        let (rounds, squares) =
            grid
            |> Array.mapi (fun y cs ->
                cs
                |> Array.mapi (fun x c ->
                    ((if c = 'O' then Some(x, y) else None), (if c = '#' then Some(x, y) else None))))
            |> Array.concat
            |> Array.unzip

        (rounds |> Array.choose id |> Set.ofArray,
         squares
         |> Array.choose id
         |> Array.map (fun p -> (p, '#'))
         |> Map.ofArray,
         grid.Length)

    let isValidPosition ((maxX, maxY): Position) (pos as (x, y): Position) (stucks: Map<Position, char>) : bool =
        if x < 0
           || y < 0
           || x > maxX
           || y > maxY
           || (stucks |> Map.containsKey pos) then
            false
        else
            true

    let moveOneStep ((x, y): Position) (dir: Direction) : Position =
        match dir with
        | N -> (x, y - 1)
        | W -> (x - 1, y)
        | S -> (x, y + 1)
        | E -> (x + 1, y)

    let sortByDirection (direction: Direction) ((x, y): Position) : int =
        match direction with
        | N -> y
        | W -> x
        | S -> -y
        | E -> -x

    let rec rollRocks
        (maxPos: Position)
        (direction: Direction)
        (movables: Position Set)
        (stucks: Map<Position, char>)
        : (Position Set) * (Map<Position, char>) =
        if movables.IsEmpty then
            (movables, stucks)
        else
            let movablesSorted =
                movables
                |> Set.toArray
                |> Array.sortBy (sortByDirection direction)

            let (movables', stucks') =
                movablesSorted
                |> Array.fold
                    (fun (mvs, sts) pos ->
                        let pos' = moveOneStep pos direction

                        if isValidPosition maxPos pos' sts then
                            ((mvs |> Set.add pos'), sts)
                        else
                            (mvs, sts |> Map.add pos 'O'))
                    (Set.empty, stucks)

            rollRocks maxPos direction movables' stucks'

    let score (height: int) (ps: Position seq) : int =
        (Seq.map (fun (_, y) -> height - y) >> Seq.sum) ps

    let part1 (input: string) : string =
        let (movables, stucks, size) = parseInput input
        let (_, stucks') = rollRocks (size - 1, size - 1) N movables stucks

        stucks'
        |> Map.filter (fun _ v -> v = 'O')
        |> Map.keys
        |> score size
        |> string

    let rec rollCycle
        (tryNr: int)
        (maxPos: Position)
        (movables: Position Set)
        (stucks: Map<Position, char>)
        (memoMap: (Position Set * (Position Set * int)) list)
        : (int * int * ((Position Set) array)) =
        match memoMap
              |> List.tryFind (fun (a, _) -> a = movables)
            with
        | Some (_, (_, i)) ->
            let cycleLength = tryNr - i
            let initialPart = tryNr - cycleLength

            (initialPart,
             cycleLength,
             memoMap
             |> List.filter (fun (_, (_, c)) -> c >= i)
             |> List.map (snd >> fst)
             |> List.toArray
             |> Array.rev)

        | None ->
            let newMovables =
                [ N; W; S; E ]
                |> List.fold
                    (fun mvs d ->
                        let (_, sts') = rollRocks maxPos d mvs stucks
                        let mvs'' = Map.filter (fun pos c -> c = 'O') sts'

                        mvs'' |> Map.keys |> Set.ofSeq)
                    movables

            rollCycle (tryNr + 1) maxPos newMovables stucks ((movables, (newMovables, tryNr)) :: memoMap)

    let part2 (input: string) : string =
        let (movables, stucks, size) = parseInput input
        let (iPart, cycle, a) = rollCycle 1 (size - 1, size - 1) movables stucks List.empty
        let index = (1000000000 - iPart) % cycle

        a[index] |> Set.toArray |> score size |> string
