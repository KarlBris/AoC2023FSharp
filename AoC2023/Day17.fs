namespace AoC2023

open Utils
open System

module Day17 =

    type PriorityQueue = ((Position*Direction) * int) list

    let addToPQ (item: (Position*Direction) * int) (queue: PriorityQueue) : PriorityQueue =
        let index =
            queue
            |> List.takeWhile (fun (_, v) -> v < (snd item))
            |> List.length

        List.insertAt index item queue

    let removeFromPQ (item: Position) (queue: PriorityQueue) : PriorityQueue =
        queue |> List.filter (fun ((p,_), _) -> p <> item)

    let makeGrid input =
        let ls =
            input
            |> lines
            |> Array.map (Array.ofSeq)
            |> Array.map (Array.map (fun c -> (int c) - (int '0')))

        let ySize = ls.Length
        let xSize = ls.[0].Length
        (xSize, ySize, array2D ls)

    let rec increaseAndWrap amount n =
        match amount with
        | 0 -> n
        | _ ->
            match n with
            | 9 -> increaseAndWrap (amount - 1) 1
            | _ -> increaseAndWrap (amount - 1) (n + 1)

    let neighbors ((x, y): Position) (multiplier: int) (grid: int [,]) : (Position*Direction) [] =
        let xSize = Array2D.length1 grid * multiplier
        let ySize = Array2D.length2 grid * multiplier

        [| ((x + 1, y), E)
           ((x - 1, y), W)
           ((x, y + 1), S)
           ((x, y - 1), N) |]
        |> Array.filter (fun ((x, y),_) -> x >= 0 && y >= 0 && x < xSize && y < ySize)

    let distance (_current: Position) (neighbor: Position) (multiplier: int) (grid: int [,]) : int =
        let (x, y) = neighbor
        let xSize = Array2D.length1 grid * multiplier
        let ySize = Array2D.length2 grid * multiplier
        let realxSize = Array2D.length1 grid
        let realySize = Array2D.length2 grid
        let xDiv = x / realxSize
        let yDiv = y / realySize
        let increases = manhattanDistance (xDiv, yDiv) (0, 0)

        if x < 0 || y < 0 || x >= xSize || y >= ySize then
            Int32.MaxValue
        else
            increaseAndWrap grid.[(x % realxSize), (y % realySize)] increases

    let rec pathToPos (cameFrom: Map<Position, Position>) (acc: (Position*Direction) list) : (Position*Direction) [] =
        let (current, _) = List.head acc

        match Map.tryFind current cameFrom with
        | Some v -> 
            let dir = whichDirection v current
            pathToPos cameFrom ((v,dir) :: acc)
        | None -> 
            let (ps, ds) = List.unzip acc
            let newAcc = Seq.zip ps (List.last ds :: ds)
            Array.ofSeq newAcc

    let findOrMax k map =
        match Map.tryFind k map with
        | Some v -> v
        | None -> Int32.MaxValue

    let pathScore (grid: int [,]) (path: Position []) : int =
        let xSize = Array2D.length1 grid
        let ySize = Array2D.length2 grid

        Array.tail path
        |> Array.map
            (fun (x, y) ->
                let xDiv = x / xSize
                let yDiv = y / ySize
                let increases = manhattanDistance (xDiv, yDiv) (0, 0)
                increaseAndWrap grid.[x % xSize, y % ySize] increases)
        |> Array.sum

    let rec checkNeighbors (neighbors: (Position*Direction) array ) current gScore multiplier grid cameFrom fScore (openSet: PriorityQueue) h goal =
        match neighbors with
        | [||] -> (cameFrom, gScore, fScore, openSet)
        | _ ->
            let (n,d) = Array.head neighbors

            let tentativeGScore =
                (Map.find current gScore)
                + (distance current n multiplier grid)

            if tentativeGScore < findOrMax n gScore then
                let cameFrom2 = Map.add n current cameFrom
                let gScore2 = Map.add n tentativeGScore gScore

                let fScore2 =
                    Map.add n (tentativeGScore + (h n goal)) fScore

                let openSet2 = addToPQ ((n,d), findOrMax n fScore2) openSet
                checkNeighbors (Array.tail neighbors) current gScore2 multiplier grid cameFrom2 fScore2 openSet2 h goal
            else
                checkNeighbors (Array.tail neighbors) current gScore multiplier grid cameFrom fScore openSet h goal

    let rec findHowManyStepsInDirection (steps:int) (cameFrom: Map<Position,Position>) (currentPos: Position) (currentDirection: Direction) : int =
        // denna är fortfarande fel! den kollar inte om det första steget tas åt samma håll som de andra...
        //let stepBackwards =  currentPos |> oneStep (currentDirection |> directionReverse)
        //match cameFrom |> Map.tryFind currentPos with
        //| None -> steps
        //| Some pos -> 
        //    if pos = stepBackwards then
        //        findHowManyStepsInDirection (steps + 1) cameFrom stepBackwards currentDirection
        //    else
        //        steps+1
        let positions = [(currentPos,currentDirection)] |> pathToPos cameFrom |> Array.rev

        positions[0..2]
        |> Array.takeWhile (fun (_,d) -> d = currentDirection)
        |> Array.length

    let printGrid (grid:int[,]) (cameFrom: Map<Position, Position>) (finalPos: Position) : unit =
        let maxSize = grid[0,*].Length

        let positions = pathToPos cameFrom [(finalPos,N)]

        [0..(maxSize-1)]
        |> List.iter (fun a -> 
            let row = grid[a,*]
            let fixedRow = 
                row
                |> Array.mapi (fun b v -> 
                    if (b,a) = finalPos then 
                        'C'
                    elif (b,a) = (0,0) then
                        v |> string |> Seq.head
                    else
                        match positions |> Array.tryFind (fun (pos,_) -> pos = (b,a)) with
                        | None -> v |> string |> Seq.head
                        | Some (_,d) ->
                            match d with
                            | N -> '^'
                            | S -> 'v'
                            | W -> '<'
                            | E -> '>'
                        
                        )
                    
            printfn $"{String.Concat fixedRow}")

    let rec aStarRunner (openSet: PriorityQueue) fScore gScore cameFrom (goal: Position) multiplier h grid =
        match openSet with
        | [] ->
            let scoremaybe =
                pathToPos cameFrom [ (goal,N) ]
                |> Array.map fst
                |> pathScore grid
            scoremaybe
            //failwith "error"
        | _ ->
            let (currentPos, currentPosDir) = fst openSet.Head

            let howManyStepsInCurrentDirection = findHowManyStepsInDirection 0 cameFrom currentPos currentPosDir

            if howManyStepsInCurrentDirection > 3 then
                printf ""

            if currentPos = goal && howManyStepsInCurrentDirection < 4 then
                printGrid grid cameFrom currentPos
                pathToPos cameFrom [ (currentPos, currentPosDir) ]
                |> Array.map fst
                |> pathScore grid
            else
                let openSet2 = removeFromPQ currentPos openSet
                let ns = (neighbors currentPos multiplier grid)
                let ns' =
                    if howManyStepsInCurrentDirection = 3 then
                        ns |> Array.filter (fun (_,d) -> d <> currentPosDir)
                    else
                        ns

                let (cameFrom2, gScore2, fScore2, openSet3) =
                    checkNeighbors ns' currentPos gScore multiplier grid cameFrom fScore openSet2 h goal

                aStarRunner openSet3 fScore2 gScore2 cameFrom2 goal multiplier h grid

    let aStar (start: Position) (goal: Position) (grid: int [,]) (h: Position -> Position -> int) (multiplier: int) =
        let openSet = [ ((start, E), h start goal) ]
        let cameFrom: Map<Position, Position> = Map.empty

        let gScore = Map.ofArray [| (start, 0) |]
        let fScore = Map.ofArray [| (start, h start goal) |]

        aStarRunner openSet fScore gScore cameFrom goal multiplier manhattanDistance grid

    let part1 (input: string) : string =
        let (xSize, ySize, grid) = input |> makeGrid

        aStar (0, 0) (xSize - 1, ySize - 1) grid manhattanDistance 1
        |> string

    ////let parseInput (input: string) : Map<Position, int> =
    ////    input
    ////    |> lines
    ////    |> Array.mapi (fun y cs -> cs |> Seq.mapi (fun x c -> ((x, y), charToInt c)))
    ////    |> Seq.concat
    ////    |> Map.ofSeq

    ////let conStepsOKCheck (dir: Direction) ((cDir,cSteps): Direction*int) : bool = 
    ////    if cDir = dir then
    ////        cSteps < 3
    ////    else
    ////        true

    ////let updateConSteps (dir: Direction) ((cDir,cSteps): Direction*int) : Direction*int = 
    ////    if cDir = dir then
    ////        (dir, (cSteps+1))
    ////    else
    ////        (dir,1)

    ////let rec roll
    ////    (costSoFar: int)
    ////    (pos: Position)
    ////    (consecutiveSteps: Direction * int)
    ////    (goalPos: Position)
    ////    (lowestCost: int)
    ////    (visiteds: Position Set)
    ////    (memoMap: Map<(Position*(Direction*int)), int>)
    ////    (costMap: Map<Position, int>)
    ////    : (int* Map<(Position*(Direction*int)), int>) =

    ////    match memoMap |> Map.tryFind (pos, consecutiveSteps) with
    ////    | Some s -> (s, memoMap)
    ////    | None -> 

    ////        if pos = goalPos || costSoFar > lowestCost then
    ////            (costSoFar, memoMap)
    ////        else
    ////            let newPositionsAndDirections = manhattanNeighborPositions |> Array.map (fun (p,d) -> ((addPos pos p), d)) |> Array.filter (fun (newPos, newD) -> 
    ////                (isValidPosition goalPos newPos)
    ////                && (visiteds |> Set.contains newPos |> not)
    ////                && (conStepsOKCheck newD consecutiveSteps))
            
    ////            let (result, memoMap') = 
    ////                newPositionsAndDirections |> Array.fold (fun (lowest', mmap) (np,nd) -> 
    ////                let newConSteps = updateConSteps nd consecutiveSteps
    ////                let (res, mmap') = roll (costSoFar + costMap[np]) np newConSteps goalPos lowest' (visiteds |> Set.add pos) mmap costMap
                
    ////                (min res lowest', mmap')

    ////                ) (lowestCost, memoMap)
                
    ////            (result, memoMap' |> Map.add (pos, consecutiveSteps) result)

    ////let part1 (input: string) : string =
    ////    let hej = input |> parseInput
    ////    let size = input |> lines |> Array.length

    ////    let initialConSteps = (E,0)

    ////    let foo = roll 0 (0, 0) initialConSteps (size - 1, size - 1) Int32.MaxValue Set.empty Map.empty hej

    ////    foo |> fst |> string


    let part2 (input: string) : string = input
