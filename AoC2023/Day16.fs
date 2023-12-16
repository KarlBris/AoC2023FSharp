namespace AoC2023

open Utils

module Day16 =
    let parseInput (input: string) : Map<Position, char> =
        input
        |> lines
        |> Array.mapi (fun y cs ->
            cs
            |> Seq.toArray
            |> Array.mapi (fun x c ->
                if c <> '.' then
                    Some((x, y), c)
                else
                    None)
            |> Array.choose id)
        |> Array.concat
        |> Map.ofArray

    let changeDirection (mirrorType: char) (dir: Direction) : Direction list =
        match mirrorType with
        | '\\' ->
            match dir with
            | N -> [ W ]
            | E -> [ S ]
            | S -> [ E ]
            | W -> [ N ]
        | '/' ->
            match dir with
            | N -> [ E ]
            | E -> [ N ]
            | S -> [ W ]
            | W -> [ S ]
        | '-' ->
            match dir with
            | N -> [ W; E ]
            | E -> [ E ]
            | S -> [ W; E ]
            | W -> [ W ]
        | '|' ->
            match dir with
            | N -> [ N ]
            | E -> [ N; S ]
            | S -> [ S ]
            | W -> [ N; S ]
        | _ -> failwith ""

    let isAInFrontOfB (a as (aX, aY): Position) (b as (bX, bY): Position) (bDir: Direction) : bool =
        match bDir with
        | N -> aY < bY
        | E -> aX > bX
        | S -> aY > bY
        | W -> aX < bX

    let isPointsInLine ((aX, aY): Position) ((bX, bY): Position) (dir: Direction) : bool =
        match dir with
        | N -> aX = bX
        | E -> aY = bY
        | S -> aX = bX
        | W -> aY = bY

    let findClosestPosInFront (pos: Position) (dir: Direction) (mirrorMap: Map<Position, char>) : Position option =
        let frontPositions =
            mirrorMap
            |> Map.keys
            |> Seq.filter (fun p ->
                (isAInFrontOfB p pos dir)
                && (isPointsInLine p pos dir))

        if frontPositions |> Seq.isEmpty then
            None
        else
            Some(
                match dir with
                | N -> frontPositions |> Seq.minBy (fun (_, y) -> -y)
                | E -> frontPositions |> Seq.minBy (fun (x, _) -> x)
                | S -> frontPositions |> Seq.minBy (fun (_, y) -> y)
                | W -> frontPositions |> Seq.minBy (fun (x, _) -> -x)
            )

    let positionsTo (fromPos as (fX, fY): Position) (toPos as (tX, tY): Position) : Position list =
        if fX = tX then
            [ for y in (min fY tY) .. (max fY tY) -> (fX, y) ]
        elif fY = tY then
            [ for x in (min fX tX) .. (max fX tX) -> (x, fY) ]
        else
            failwith ""

    let farthestPos ((x, y): Position) ((maxX, maxY): Position) (dir: Direction) : Position =
        match dir with
        | N -> (x, -1)
        | S -> (x, maxX + 1)
        | W -> (-1, y)
        | E -> (maxX + 1, y)

    //TODO: memoize this shit
    let rec beamStuff
        (pos: Position)
        (maxPos: Position)
        (dir: Direction)
        (energizedPositions: (Position * Direction) Set)
        (mirrorMap: Map<Position, char>)
        : (Position * Direction) Set =
        if energizedPositions
           |> Set.exists (fun (p, d) -> p = pos && d = dir) then
            energizedPositions
        else if pos |> isValidPosition maxPos then
            match mirrorMap |> Map.tryFind pos with
            | None ->
                let newPosition =
                    match findClosestPosInFront pos dir mirrorMap with
                    | Some p -> p
                    | None -> farthestPos pos maxPos dir

                let energizedPositions' =
                    (positionsTo pos newPosition)
                    |> List.map (fun a -> (a, dir))
                    |> Set.ofList
                    |> Set.union energizedPositions
                    |> Set.remove (newPosition, dir)

                beamStuff newPosition maxPos dir energizedPositions' mirrorMap
            | Some c ->
                let energizedPositions' = Set.add (pos, dir) energizedPositions
                let dirs = changeDirection c dir

                dirs
                |> List.fold (fun s v -> beamStuff (oneStep v pos) maxPos v s mirrorMap) energizedPositions'
        else
            energizedPositions

    let part1 (input: string) : string =
        let mirrorMap = input |> parseInput
        let size = input |> lines |> Array.length

        mirrorMap
        |> beamStuff (0, 0) (size - 1, size - 1) E Set.empty
        |> Set.map (fst)
        |> Set.filter (fun pos -> isValidPosition (size - 1, size - 1) pos)
        |> Set.count
        |> string

    let part2 (input: string) : string =
        let mirrorMap = input |> parseInput

        let size = input |> lines |> Array.length

        let topPositions = [ for x in 0 .. (size - 1) -> ((x, 0), S) ]
        let bottomPositions = [ for x in 0 .. (size - 1) -> ((x, (size - 1)), N) ]
        let leftPositions = [ for y in 0 .. (size - 1) -> ((0, y), E) ]
        let rightPositions = [ for y in 0 .. (size - 1) -> (((size - 1), y), W) ]

        [ topPositions
          bottomPositions
          leftPositions
          rightPositions ]
        |> List.concat
        |> List.map (fun (p, d) ->
            mirrorMap
            |> beamStuff p (size - 1, size - 1) d Set.empty
            |> Set.map (fst)
            |> Set.count)
        |> List.max
        |> string
