namespace AoC2023

open System.IO
open System.Diagnostics
open System

module TestRunner =

    let getInput day =
        let filename = $"..\..\..\inputs\input_day{day}.txt"

        if File.Exists filename then
            filename
            |> File.ReadAllText
            |> String.filter (fun c -> c <> '\r')
        else
            failwith $"Input file {filename} not found"

    let makeComparison (expectedResults: string []) (results: string []) =
        Array.zip results expectedResults
        |> Array.map (fun (r, e) -> (r, e, r = e))

    let printStatus ((res, expectedRes, success): string * string * bool) =
        printfn "%s! Got %s, expected %s." (if success then "Success" else "Failure") res expectedRes

    let formatTime (span: TimeSpan) : string =
        $"{span.Hours} Hours, {span.Minutes} Minutes, {span.Seconds} Seconds, {span.Milliseconds}.{span.Microseconds} Milliseconds"

    let run (examples: string []) expectedResults realInput (func: string -> string) title =
        printfn title

        if examples.Length = 0 then
            printfn "No examples found, running the real input..."
        else
            printfn "Running and verifying examples before the real input..."

        let resultList =
            examples
            |> Array.map func
            |> makeComparison expectedResults

        resultList |> Array.map printStatus |> ignore

        let examplesSuccessful =
            resultList
            |> Array.fold (fun b1 (_, _, b2) -> b1 && b2) true

        if examplesSuccessful then
            printfn "All examples were successful, running the real input..."
            let timer = new Stopwatch()
            timer.Start()
            printfn "Result from real input: %s" (func realInput)
            timer.Stop()
            printfn $"Time elapsed: {(formatTime timer.Elapsed)}"
            printfn "Time elapsed: %A" timer.Elapsed
        else
            printfn "Some examples were not successful. PLEASE DO BETTER"

        printfn ""

    // Day1
    let input1 = getInput 1

    let examples1_1 =
        [| "1abc2\npqr3stu8vwx\na1b2c3d4e5f\ntreb7uchet" |]

    let exampleResults1_1 = [| "142" |]

    let examples1_2 =
        [| "two1nine\neightwothree\nabcone2threexyz\nxtwone3four\n4nineeightseven2\nzoneight234\n7pqrstsixteen" |]

    let exampleResults1_2 = [| "281" |]

    // Day2
    let input2 = getInput 2

    let examples2_1 =
        [| "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\nGame 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\nGame 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\nGame 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\nGame 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green" |]

    let exampleResults2_1 = [| "8" |]

    let examples2_2 = examples2_1

    let exampleResults2_2 = [| "2286" |]

    // Day3
    let input3 = getInput 3

    let examples3_1 = [| "467..114..\n...*......\n..35..633.\n......#...\n617*......\n.....+.58.\n..592.....\n......755.\n...$.*....\n.664.598.." |]

    let exampleResults3_1 = [| "4361" |]

    let examples3_2 =  examples3_1

    let exampleResults3_2 = [| "467835" |]

    // Day4
    let input4 = getInput 4

    let examples4_1 = [| "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53\nCard 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19\nCard 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1\nCard 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83\nCard 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36\nCard 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11" |]

    let exampleResults4_1 = [| "13" |]

    let examples4_2 =  examples4_1

    let exampleResults4_2 = [| "30" |]

    // Day5
    let input5 = getInput 5

    let examples5_1 = [| "seeds: 79 14 55 13\n\nseed-to-soil map:\n50 98 2\n52 50 48\n\nsoil-to-fertilizer map:\n0 15 37\n37 52 2\n39 0 15\n\nfertilizer-to-water map:\n49 53 8\n0 11 42\n42 0 7\n57 7 4\n\nwater-to-light map:\n88 18 7\n18 25 70\n\nlight-to-temperature map:\n45 77 23\n81 45 19\n68 64 13\n\ntemperature-to-humidity map:\n0 69 1\n1 0 69\n\nhumidity-to-location map:\n60 56 37\n56 93 4" |]

    let exampleResults5_1 = [| "35" |]

    let examples5_2 =  examples5_1

    let exampleResults5_2 = [| "46" |]

    // Day6
    let input6 = getInput 6

    let examples6_1 = [| "Time:      7  15   30\nDistance:  9  40  200" |]

    let exampleResults6_1 = [| "288" |]

    let examples6_2 =  examples6_1

    let exampleResults6_2 = [| "71503" |]

    // Day7
    let input7 = getInput 7

    let examples7_1 = [| "32T3K 765\nT55J5 684\nKK677 28\nKTJJT 220\nQQQJA 483" |]

    let exampleResults7_1 = [| "6440" |]

    let examples7_2 = examples7_1

    let exampleResults7_2 = [| "5905" |]

    // Day8
    let input8 = getInput 8

    let examples8_1 = [| "RL\n\nAAA = (BBB, CCC)\nBBB = (DDD, EEE)\nCCC = (ZZZ, GGG)\nDDD = (DDD, DDD)\nEEE = (EEE, EEE)\nGGG = (GGG, GGG)\nZZZ = (ZZZ, ZZZ)"; "LLR\n\nAAA = (BBB, BBB)\nBBB = (AAA, ZZZ)\nZZZ = (ZZZ, ZZZ)" |]

    let exampleResults8_1 = [| "2"; "6" |]

    let examples8_2 = [| "LR\n\n11A = (11B, XXX)\n11B = (XXX, 11Z)\n11Z = (11B, XXX)\n22A = (22B, XXX)\n22B = (22C, 22C)\n22C = (22Z, 22Z)\n22Z = (22B, 22B)\nXXX = (XXX, XXX)" |]

    let exampleResults8_2 = [| "6" |]

    // Day9
    let input9 = getInput 9

    let examples9_1 = [| "0 3 6 9 12 15\n1 3 6 10 15 21\n10 13 16 21 30 45" |]

    let exampleResults9_1 = [| "114" |]

    let examples9_2 = examples9_1

    let exampleResults9_2 = [| "2" |]

    // Day10
    let input10 = getInput 10

    let examples10_1 = [| ".....\n.S-7.\n.|.|.\n.L-J.\n....."; "..F7.\n.FJ|.\nSJ.L7\n|F--J\nLJ..." |]

    let exampleResults10_1 = [| "4"; "8" |]

    let examples10_2 = [| "...........\n.S-------7.\n.|F-----7|.\n.||.....||.\n.||.....||.\n.|L-7.F-J|.\n.|..|.|..|.\n.L--J.L--J.\n..........."; "..........\n.S------7.\n.|F----7|.\n.||OOOO||.\n.||OOOO||.\n.|L-7F-J|.\n.|II||II|.\n.L--JL--J.\n.........."; ".F----7F7F7F7F-7....\n.|F--7||||||||FJ....\n.||.FJ||||||||L7....\nFJL7L7LJLJ||LJ.L-7..\nL--J.L7...LJS7F-7L7.\n....F-J..F7FJ|L7L7L7\n....L7.F7||L7|.L7L7|\n.....|FJLJ|FJ|F7|.LJ\n....FJL-7.||.||||...\n....L---J.LJ.LJLJ..."; "FF7FSF7F7F7F7F7F---7\nL|LJ||||||||||||F--J\nFL-7LJLJ||||||LJL-77\nF--JF--7||LJLJ7F7FJ-\nL---JF-JLJ.||-FJLJJ7\n|F|F-JF---7F7-L7L|7|\n|FFJF7L7F-JF7|JL---7\n7-L-JL7||F7|L7F-7F7|\nL.L7LFJ|||||FJL7||LJ\nL7JLJL-JLJLJL--JLJ.L" |]

    let exampleResults10_2 = [| "4"; "4"; "8"; "10" |]

    // Day11
    let input11 = getInput 11

    let examples11_1 = [| "...#......\n.......#..\n#.........\n..........\n......#...\n.#........\n.........#\n..........\n.......#..\n#...#....." |]

    let exampleResults11_1 = [| "374" |]

    let examples11_2 = [| |]

    let exampleResults11_2 = [| |]

    // Day12
    let input12 = getInput 12

    let examples12_1 = [|"???.### 1,1,3";".??..??...?##. 1,1,3";"?#?#?#?#?#?#?#? 1,3,1,6";"????.#...#... 4,1,1";"????.######..#####. 1,6,5";"?###???????? 3,2,1"|]

    let exampleResults12_1 = [| "1";"4";"1";"1";"4";"10"; |]

    let examples12_2 = examples12_1

    let exampleResults12_2 = [| "1";"16384";"1";"16";"2500";"506250"; |]

    // Day13
    let input13 = getInput 13

    let examples13_1 = [|"#.##..##.\n..#.##.#.\n##......#\n##......#\n..#.##.#.\n..##..##.\n#.#.##.#.\n\n#...##..#\n#....#..#\n..##..###\n#####.##.\n#####.##.\n..##..###\n#....#..#"|]

    let exampleResults13_1 = [|"405"|]

    let examples13_2 = examples13_1

    let exampleResults13_2 = [|"400"|]

    // Day14
    let input14 = getInput 14

    let examples14_1 = [|"O....#....\nO.OO#....#\n.....##...\nOO.#O....O\n.O.....O#.\nO.#..O.#.#\n..O..#O..O\n.......O..\n#....###..\n#OO..#...."|]

    let exampleResults14_1 = [|"136"|]

    let examples14_2 = examples14_1

    let exampleResults14_2 = [|"64"|]

    // Day15
    let input15 = getInput 15

    let examples15_1 = [|"rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"|]

    let exampleResults15_1 = [|"1320"|]

    let examples15_2 = examples15_1

    let exampleResults15_2 = [|"145"|]

    // Day16
    let input16 = getInput 16

    let examples16_1 = [|".|...\\....\n|.-.\\.....\n.....|-...\n........|.\n..........\n.........\\\n..../.\\\\..\n.-.-/..|..\n.|....-|.\\\n..//.|...."|]

    let exampleResults16_1 = [|"46"|]

    let examples16_2 = examples16_1

    let exampleResults16_2 = [|"51"|]

    // Day17
    let input17 = getInput 17

    let examples17_1 = [|"2413432311323\n3215453535623\n3255245654254\n3446585845452\n4546657867536\n1438598798454\n4457876987766\n3637877979653\n4654967986887\n4564679986453\n1224686865563\n2546548887735\n4322674655533"|]

    let exampleResults17_1 = [|"102"|]

    let examples17_2 = [|""|]

    let exampleResults17_2 = [|""|]