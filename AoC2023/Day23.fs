namespace AoC2023

open Utils

module Day23 =
    
    type TrailMap = Map<Position, char>

    let parseInput (input:string) : TrailMap =
        input 
        |> lines
        |> Array.mapi (fun y cs -> 
            cs
            |> Seq.mapi (fun x c -> if c = '#' then None else Some ((x,y),c))
            )
        |> Seq.concat
        |> Seq.choose id
        |> Map.ofSeq

    let directionMatchesSlope (dir:Direction) (slope:char) : bool = 
        match (dir, slope) with
        | (_, '.')
        | (N, '^')
        | (S, 'v')
        | (W, '<')
        | (E, '>') -> true
        | _ -> false

    let rec lengthOfLongestPath (part2: bool) (stepsTaken:int) (pos:Position) (endPos: Position) (visiteds:Position Set) (trailMap:TrailMap) : int =
        if pos = endPos then
            stepsTaken
        else
            let potentialPositions = 
                manhattanNeighborPositions |> Array.map (fun (p,d) -> ((addPos pos p), d))
                |> Array.filter (fun (p,d) -> (trailMap |> Map.containsKey p) && (visiteds |> Set.contains p |> not) && (part2 || (directionMatchesSlope d trailMap[p])))
                |> Array.map fst

            if potentialPositions.Length = 1 then
                lengthOfLongestPath part2 (stepsTaken + 1) (potentialPositions[0]) endPos (visiteds |> Set.add pos) trailMap
            elif potentialPositions.Length > 1 then
                let pathLengths = potentialPositions |> Array.map (fun p -> lengthOfLongestPath part2 (stepsTaken + 1) (p) endPos (visiteds |> Set.add pos) trailMap)
                pathLengths |> Array.max
            else
                -1337

    let rec exploreEdge (stepsTaken:int) (pos:Position) (endPositions: Position list) (visiteds:Position Set) (trailMap:TrailMap) : (int*Position) =
        if endPositions |> List.contains pos then
            (stepsTaken, pos)
        else
            let potentialPositions = 
                manhattanNeighborPositions |> Array.map (fun (p,d) -> (addPos pos p))
                |> Array.filter (fun (p) -> (trailMap |> Map.containsKey p) && (visiteds |> Set.contains p |> not))

            if potentialPositions.Length = 1 then
                exploreEdge (stepsTaken + 1) (potentialPositions[0]) endPositions (visiteds |> Set.add pos) trailMap
            else
                failwith ""

    let traverseTrails (part2:bool) (trailMap:TrailMap) : int =
        let startPos = trailMap |> Map.keys |> Seq.minBy (fun (x,y) -> y)
        let endPos = trailMap |> Map.keys |> Seq.maxBy (fun (x,y) -> y)
        lengthOfLongestPath part2 0 startPos endPos Set.empty trailMap
        
    let part1 (input: string) : string = 
        input
        |> parseInput 
        |> traverseTrails false
        |> string

    let populateEdgesRunner (startPosition: Position) (endPositions: Position list) (trailMap:TrailMap): ((Position Set)* int) list  =
        manhattanNeighborPositions 
        |> Array.map (fun (p,d) -> ((addPos p startPosition), d))
        |> Array.filter (fun (p,d) -> (trailMap |> Map.containsKey p))
        |> Array.map fst
        |> List.ofArray
        |> List.map (fun p -> 
            let (dist, endPos) = exploreEdge 1 p endPositions (Set.singleton startPosition) trailMap
            (Set.ofList [startPosition; endPos]), dist)

    let populateEdges (startPos: Position) (endPos: Position) (trailMap: TrailMap) : Map<(Position Set), int> =
        let crossroads = 
            trailMap 
            |> Map.keys 
            |> Seq.filter (fun pos -> 
                let hej = 
                    manhattanNeighborPositions 
                    |> Array.map (fun (p,d) -> ((addPos p pos), d))
                    |> Array.filter (fun (p,d) -> (trailMap |> Map.containsKey p))
                    |> Array.map fst
            
                hej.Length > 2)
            |> List.ofSeq

        let totalPoints = [[startPos]; crossroads; [endPos]] |> List.concat

        totalPoints
        |> List.map (fun p -> populateEdgesRunner p (totalPoints) trailMap)
        |> List.concat
        |> Map.ofList

    let rec traverseGraph (startPos: Position) (endPos: Position) (edgeMap: Map<Position, ((Position Set) list)>) (visiteds: Position Set) (graph: Map<(Position Set), int>) : int =
        if startPos = endPos then
            0
        else
            let edges = edgeMap[startPos] |> List.filter (fun pSet -> Set.intersect pSet visiteds |> Set.isEmpty)
            if edges.Length = 0 then
                -50000
            else
                edges 
                |> List.map (fun ps -> 
                    let targetPos = ps |> Set.remove startPos |> Seq.head
                    let thisEdgeCost = graph[ps]
                    let traversedCost = (traverseGraph targetPos endPos edgeMap (visiteds |> Set.add startPos) graph)
                    thisEdgeCost + traversedCost
                    )
                |> List.max

    let makeEdgeMap (graph: Map<(Position Set), int>) : Map<Position, ((Position Set) list)> =
        let nodes = 
            graph
            |> Map.keys
            |> List.ofSeq
            |> List.collect Set.toList
            |> List.distinct

        nodes
        |> List.map (fun n -> (n, graph |> Map.keys |> Seq.toList |> List.filter (fun x -> x.Contains n)))
        |> Map.ofList

    let part2 (input: string) : string = 
        
        let trailMap = parseInput input
        
        let startPos = trailMap |> Map.keys |> Seq.minBy (fun (_,y) -> y)
        let endPos = trailMap |> Map.keys |> Seq.maxBy (fun (_,y) -> y)

        let graph = populateEdges startPos endPos trailMap

        let edges = makeEdgeMap graph

        traverseGraph startPos endPos edges Set.empty graph |> string
