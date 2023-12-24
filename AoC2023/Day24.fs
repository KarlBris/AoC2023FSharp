namespace AoC2023

open Utils

module Day24 =
    
    type PosVel = (int64*int64*int64) * (int64*int64*int64)
    type LineSeg = (int64*int64*int64) * (int64*int64*int64)

    let parseInputLine (input:string) : PosVel =
        input
        |> split "@"
        |> Array.map (fun s -> 
            s
            |> commas
            |> Array.map int64
            |> (fun is -> (is[0], is[1], is[2])))
        |> twoArrayToTuple

    let extendToLineSegment (size:int64) (((x,y,z),(vx,vy,vz)): PosVel) : LineSeg =
        ((x,y,z),(x+(vx*size), y+(vy*size), z+(vz*size)))

    let parametric (((x1,y1,_),(x2,y2,_)): LineSeg) (v: double) : (double*double) =
        ((double x1)+(v*((double x2)-(double x1))),(double y1)+(v*((double y2)-(double y1))))
        

    let findIntersectionXY (((x1,y1,_),(x2,y2,_)) as lsA: LineSeg) (((x3,y3,_),(x4,y4,_)) as lsB: LineSeg) : (double*double) Option =
        let paramS = parametric lsA
        //let paramT = parametric lsB

        // TODO: check parallel somehow

        let a = x2-x1 |> double
        let b = x4-x3 |> double
        let c = x3-x1 |> double
        let d = y2-y1 |> double
        let e = y4-y3 |> double
        let f = y3-y1 |> double

        let denom = (d-((a*e)/b))
        if denom = 0 then
            None
        else
            
            let s0 = (f-((c*e)/b))/denom
            let t0 = (f-((c*d)/a))/(((b*d)/a)-e)
            let solution = paramS s0

            if s0 >=0 && s0 <= 1 && t0 >=0 && t0 <= 1 then

                Some solution

            else
                None

    let rec checkAllIntersections (minVal: double) (maxVal: double) (lineSegs: LineSeg list) : int =
        match lineSegs with
        | l::ls -> (ls |> List.fold (fun n ls ->
            match findIntersectionXY l ls with
            | None -> n
            | Some (x,y) -> 
                if x >= minVal && x <= maxVal && y >= minVal && y <= maxVal then 
                    1+n 
                else 
                    n) 0) + (checkAllIntersections minVal maxVal ls)
        | _ -> 0
        

    let part1 (input: string) : string = 

        let hej = 
            input 
            |> lines
            |> Array.map parseInputLine

        let (minSize, maxSize) = if hej.Length < 10 then (7L, 27L) else (200000000000000L, 400000000000000L)

        let lineSegs = hej |> Array.map (extendToLineSegment (maxSize-minSize))

        let intersection = findIntersectionXY lineSegs[2] lineSegs[4] 
        
        let intersections = 
            lineSegs
            |> List.ofArray
            |> checkAllIntersections (double minSize) (double maxSize)


        intersections |> string

    let part2 (input: string) : string = 
        input