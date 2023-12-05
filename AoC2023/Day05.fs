namespace AoC2023

open Utils
open System

module Day05 =

    type Input1 = (int64 array * ((int64*int64*int64) array) array)
    type Input2 = ((int64 * int64) array * ((int64*int64*int64) array) array)

    let parseInput1 (input: string) : Input1 =

        let hej = input |> splitAtDoubleLines
        let seeds = hej[0]|>colons|> (fun a -> a[1])|>words|> Array.map int64
        let maps = hej |> Array.tail |> Array.map (fun mapLines -> mapLines |> lines |> Array.tail |> Array.map (fun mapRow -> mapRow |> words |> Array.map int64 |> (fun x -> (x[0], x[1], x[2]))))

        (seeds, maps)   

    let parseInput2 (input: string) : Input2 =

        let hej = input |> splitAtDoubleLines
        let seeds = hej[0]|>colons|> (fun a -> a[1])|>words|> Array.chunkBySize 2 |> Array.map (fun n -> (int64 n[0], int64 n[1]))
        let maps = hej |> Array.tail |> Array.map (fun mapLines -> mapLines |> lines |> Array.tail |> Array.map (fun mapRow -> mapRow |> words |> Array.map int64 |> (fun x -> (x[0], x[1], x[2]))))

        (seeds, maps)        

    let mapPryl ((dest, source, length): (int64*int64*int64)) (range: (bool*(int64*Int64))) : (bool*(int64*Int64)) array =
        
        let (mapped, (sStart, sLength)) = range

        if mapped 
        then
            [|range|]
        else
            let sEnd = sStart + sLength-1L
            if 
                sStart >= source && sEnd <= (source + length)
            then// case total overlap, map everything
                let diff = sStart - source
                [|(true, (dest + diff, sLength))|]
            elif sStart < source && sEnd >= source && sEnd <= (source + length) // partial overlap, range lower
            then
                let outsideLength = source - sStart - 1L
                [|(false, (sStart, (outsideLength)));(true,(dest, sLength - outsideLength))|]
            elif sStart >= source && sStart <= (source + length) && sEnd > (source + length) // partial overlap, range higher
            then
                let insideLength = (length) - (sStart - source)
                let diff = sStart - source
                [|(true,(dest + diff, insideLength));(false,(source+length+1L,sLength-insideLength))|]
            elif sStart < source && sEnd > (source + length)  // range juts out on both sides!
            then
                [|(false,(sStart, (source-sStart-1L)));(true,(dest, length));(false,(source+length, sLength-((source+length-1L)-sStart)-1L))|]
            else // no overlap, map nothing
                [|range|]
        

    let mapValueRange (currentMap: (int64*int64*int64) array) (numRange: (int64* int64)) : (int64 * int64) array =
        currentMap
            |> Array.fold (fun numberRanges map -> 
                numberRanges
                |> Array.map (mapPryl map)
                |> Array.concat

                ) [|(false, numRange)|]
        |> Array.map snd

    let mapValue (currentMap: (int64*int64*int64) array) (num: int64) : int64 =
        currentMap
        |> Array.fold (fun (mapped,number) (dest, source, length) -> 
            
            if number >= source && number <= source+length && not mapped
            then 
                let diff = number - source
                let mappedNum = dest + diff
                (true,mappedNum)
            else (mapped,number)) (false, num)
        |> snd
   
    let part1 (input: string) : string =
        let (seeds, maps) = input |> parseInput1

        maps
        |> Array.fold (fun seedNums currentMap -> Array.map (fun seedNum -> mapValue currentMap seedNum) seedNums ) seeds
        |> Array.min
        |> string

    let part2 (input: string) : string =
        let (seeds, maps) = input |> parseInput2

        maps
        |> Array.fold (fun valueRanges currentMap -> 
            valueRanges
            |> Array.map (mapValueRange currentMap)
            |> Array.concat
            
            ) seeds
        |> Array.map fst
        |> Array.min
        |> string