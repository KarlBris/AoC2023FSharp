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

    // Day18
    let input18 = getInput 18

    let examples18_1 = [|"R 6 (#70c710)\nD 5 (#0dc571)\nL 2 (#5713f0)\nD 2 (#d2c081)\nR 2 (#59c680)\nD 2 (#411b91)\nL 5 (#8ceee2)\nU 2 (#caa173)\nL 1 (#1b58a2)\nU 2 (#caa171)\nR 2 (#7807d2)\nU 3 (#a77fa3)\nL 2 (#015232)\nU 2 (#7a21e3)"|]

    let exampleResults18_1 = [|"62"|]

    let examples18_2 = examples18_1

    let exampleResults18_2 = [|"952408144115"|]

    // Day19
    let input19 = getInput 19

    let examples19_1 = [|"px{a<2006:qkq,m>2090:A,rfg}\npv{a>1716:R,A}\nlnx{m>1548:A,A}\nrfg{s<537:gd,x>2440:R,A}\nqs{s>3448:A,lnx}\nqkq{x<1416:A,crn}\ncrn{x>2662:A,R}\nin{s<1351:px,qqz}\nqqz{s>2770:qs,m<1801:hdj,R}\ngd{a>3333:R,R}\nhdj{m>838:A,pv}\n\n{x=787,m=2655,a=1222,s=2876}\n{x=1679,m=44,a=2067,s=496}\n{x=2036,m=264,a=79,s=2244}\n{x=2461,m=1339,a=466,s=291}\n{x=2127,m=1623,a=2188,s=1013}"|]

    let exampleResults19_1 = [|"19114"|]

    let examples19_2 = examples19_1

    let exampleResults19_2 = [|"167409079868000"|]

    // Day20
    let input20 = getInput 20

    let examples20_1 = [|"broadcaster -> a, b, c\n%a -> b\n%b -> c\n%c -> inv\n&inv -> a";"broadcaster -> a\n%a -> inv, con\n&inv -> b\n%b -> con\n&con -> output"|]

    let exampleResults20_1 = [|"32000000";"11687500"|]

    let examples20_2 = [||]

    let exampleResults20_2 = [||]

    // Day22
    let input22 = getInput 22

    let examples22_1 = [|"1,0,1~1,2,1\n0,0,2~2,0,2\n0,2,3~2,2,3\n0,0,4~0,2,4\n2,0,5~2,2,5\n0,1,6~2,1,6\n1,1,8~1,1,9"|]

    let exampleResults22_1 = [|"5"|]

    let examples22_2 = examples22_1

    let exampleResults22_2 = [|"7"|]

    // Day23
    let input23 = getInput 23

    let examples23_1 = [|"#.#####################\n#.......#########...###\n#######.#########.#.###\n###.....#.>.>.###.#.###\n###v#####.#v#.###.#.###\n###.>...#.#.#.....#...#\n###v###.#.#.#########.#\n###...#.#.#.......#...#\n#####.#.#.#######.#.###\n#.....#.#.#.......#...#\n#.#####.#.#.#########v#\n#.#...#...#...###...>.#\n#.#.#v#######v###.###v#\n#...#.>.#...>.>.#.###.#\n#####v#.#.###v#.#.###.#\n#.....#...#...#.#.#...#\n#.#########.###.#.#.###\n#...###...#...#...#.###\n###.###.#.###v#####v###\n#...#...#.#.>.>.#.>.###\n#.###.###.#.###.#.#v###\n#.....###...###...#...#\n#####################.#"|]

    let exampleResults23_1 = [|"94"|]

    let examples23_2 = examples23_1

    let exampleResults23_2 = [|"154"|]