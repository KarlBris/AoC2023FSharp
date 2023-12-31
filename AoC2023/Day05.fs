﻿namespace AoC2023

open Utils
open System

module Day05 =

    type Input = ((int64 * int64) array * ((int64 * int64 * int64) array) array)

    let parseInput (part1: bool) (input: string) : Input =

        let almanacMaps = input |> splitAtDoubleLines

        let seedsPart =
            almanacMaps[0]
            |> colons
            |> (fun a -> a[1])
            |> words

        let seeds =
            if part1 then
                seedsPart |> Array.map (fun n -> (int64 n, 1L))
            else
                seedsPart
                |> Array.chunkBySize 2
                |> Array.map (fun n -> (int64 n[0], int64 n[1]))

        let maps =
            almanacMaps
            |> Array.tail
            |> Array.map (fun mapLines ->
                mapLines
                |> lines
                |> Array.tail
                |> Array.map (fun mapRow ->
                    mapRow
                    |> words
                    |> Array.map int64
                    |> (fun x -> (x[0], x[1], x[2]))))

        (seeds, maps)

    let mapSingleRange
        ((dest, source, length): (int64 * int64 * int64))
        (range: (bool * (int64 * Int64)))
        : (bool * (int64 * Int64)) array =

        let (mapped, (sStart, sLength)) = range

        if mapped then
            [| range |]
        else
            let sEnd = sStart + sLength - 1L
            let sourceEnd = source + length - 1L

            if sStart >= source && sEnd <= sourceEnd then // case total overlap, map everything
                let diff = sStart - source
                [| (true, (dest + diff, sLength)) |]
            elif sStart < source
                 && sEnd >= source
                 && sEnd <= sourceEnd // partial overlap, range lower
            then
                let outsideLength = source - sStart - 1L

                [| (false, (sStart, (outsideLength)))
                   (true, (dest, sLength - outsideLength)) |]
            elif sStart >= source
                 && sStart <= sourceEnd
                 && sEnd >= sourceEnd // partial overlap, range higher
            then
                let insideLength = (length) - (sStart - source)
                let diff = sStart - source

                [| (true, (dest + diff, insideLength))
                   (false, (source + length, sLength - insideLength)) |]
            elif sStart < source && sEnd > sourceEnd // range juts out on both sides!
            then
                [| (false, (sStart, (source - sStart - 1L)))
                   (true, (dest, length))
                   (false, (source + length, sLength - (sourceEnd - sStart) - 1L)) |]
            else // no overlap, map nothing
                [| range |]


    let mapValueRanges (currentMap: (int64 * int64 * int64) array) (numRange: (int64 * int64)) : (int64 * int64) array =
        currentMap
        |> Array.fold
            (fun numberRanges map ->
                numberRanges
                |> Array.map (mapSingleRange map)
                |> Array.concat

                )
            [| (false, numRange) |]
        |> Array.map snd

    let doThing ((seeds, maps): Input) : string =
        maps
        |> Array.fold
            (fun valueRanges currentMap ->
                valueRanges
                |> Array.map (mapValueRanges currentMap)
                |> Array.concat

                )
            seeds
        |> Array.map fst
        |> Array.min
        |> string

    let part1 (input: string) : string = input |> (parseInput true) |> doThing

    let part2 (input: string) : string = input |> (parseInput false) |> doThing
