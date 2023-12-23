namespace AoC2023

open Utils
open System

module Day12 =

    type Input = (string * (int array))

    type LineContainer = (char array) * (int list)

    let parseInputLine (input: string) : Input =
        input
        |> words
        |> (fun a -> (a[0], a[1] |> commas |> Array.map int))

    let rec isPartialAnswerValid (partialAnswers: int list) (numbers as (n :: ns): int list) : bool =
        match partialAnswers with
        | [] -> true
        | a :: [] ->
            if numbers.Length = 0 then
                false
            else
                a <= n
        | a :: pa ->
            if numbers.Length > 0 then
                (if a = n then
                     isPartialAnswerValid pa ns
                 else
                     false)
            else
                false

    let prune ((line, numbers): LineContainer) : (LineContainer * bool) option =
        // can we match the beginning of the string to the first number in numbers?
        // if we can, lose the number in numbers and slice off the start of the string
        // do this recursively?
        let allHashCountAndQuestionmark =
            line
            |> Array.filter (fun c -> c = '#' || c = '?')
            |> Array.length

        let allHashCount =
            line
            |> Array.filter (fun c -> c = '#')
            |> Array.length

        let numSum = numbers |> List.sum

        // needs to have periodsLength > 0!
        let hashesLength =
            line
            |> Array.skipWhile (fun c -> c = '.')
            |> Array.takeWhile (fun c -> c = '#')
            |> Array.length

        let hashesOrQuestionmarksLength =
            line
            |> Array.skipWhile (fun c -> c = '.')
            |> Array.takeWhile (fun c -> c = '#' || c = '?')
            |> Array.length

        let questionmarksLength =
            line
            |> Array.skipWhile (fun c -> c = '.')
            |> Array.takeWhile (fun c -> c = '?')
            |> Array.length

        let periodsLength =
            line
            |> Array.takeWhile (fun c -> c = '.')
            |> Array.length

        let startStringLength = hashesLength + periodsLength

        let hasIndependentHashes =
            if startStringLength >= (line |> Array.length) then
                false
            else
                line[startStringLength] = '.'

        // if numbers is empty:
        // if line has no hashes:
        // Some empty
        // else:
        //None
        // else:
        // if line has no hashes:
        //None
        // else:
        //prune that shit

        if allHashCount > numSum then
            None
        else if numSum > allHashCountAndQuestionmark then
            None
        else

        if allHashCountAndQuestionmark = 0 && numbers.IsEmpty then
            Some(([||], []), true)
        elif questionmarksLength > 0 && numbers.IsEmpty then
            Some(([||], []), true)
        else if periodsLength = 0 then
            None
        else
            match numbers with
            | [] ->
                if allHashCountAndQuestionmark = 0 then
                    Some(([||], []), false)
                else
                    None
            | n :: ns ->
                if allHashCountAndQuestionmark = 0 then
                    None
                else if allHashCountAndQuestionmark < n then
                    None
                elif hashesLength = n then
                    Some((line[startStringLength..], ns), false)
                elif hashesLength > n then
                    None
                elif hasIndependentHashes && hashesLength < n then
                    None
                elif hashesOrQuestionmarksLength < n then
                    Some((line, numbers), false)
                else
                    Some((line, numbers), false)

    let validateStringAdvanced (numbers: int list) (fixedString: char seq) : bool =
        let firstPart =
            fixedString
            |> Seq.takeWhile (fun c -> c <> '?')
            |> String.Concat

        let partialAnswerArray =
            firstPart
            |> split "."
            |> Array.map (fun s -> s.Length)
            |> Array.toList

        if partialAnswerArray = numbers then
            true
        else
            isPartialAnswerValid partialAnswerArray numbers

    let rec hej (fixedString: char list) : char list list =
        match fixedString with
        | [] -> []
        | a ->
            if List.head a = '#' then
                (List.takeWhile ((=) '#') a)
                :: (hej ((List.skipWhile ((=) '#') a)))
            else
                (hej ((List.skipWhile ((=) '.') a)))

    let validateStringAdvanced2 (numbers: int list) (fixedString: char list) : bool =
        let firstPart = fixedString |> List.takeWhile (fun c -> c <> '?')

        let numberOfUnknowns =
            fixedString
            |> List.filter ((=) '?')
            |> List.length

        let numberOfHashes =
            fixedString
            |> List.filter ((=) '#')
            |> List.length

        let numSum = numbers |> List.sum
        let hashesLeftToUse = numSum - numberOfHashes


        if numberOfHashes > numSum then
            false
        elif hashesLeftToUse > numberOfUnknowns then
            false
        else

            let split = hej firstPart

            let partialAnswerArray = split |> List.map (fun s -> s.Length)

            isPartialAnswerValid partialAnswerArray numbers

    //let rec permuteArrangement (numberOfHashes: int) (numberOfUnknowns: int) (numbers: int array) (inputString: char array) : string list =
    //    match Array.tryFindIndex (fun c -> c= '?') inputString with
    //    | None -> [inputString |> String.Concat]
    //    | Some i ->
    //        let periodString = Array.updateAt i '.' inputString
    //        let hashString = Array.updateAt i '#' inputString
    //        let numSum = numbers |> Array.sum
    //        let hashesLeftToUse = numSum - numberOfHashes

    //        let numbers = numbers |> Array.toList

    //        let periodStrings = if (numberOfUnknowns <= hashesLeftToUse) then [] else if validateStringAdvanced numbers periodString then permuteArrangement numberOfHashes (numberOfUnknowns-1) numbers periodString else []
    //        let hashStrings = if numberOfHashes >= numSum then [] else if validateStringAdvanced numbers hashString then permuteArrangement (numberOfHashes+1) (numberOfUnknowns-1) numbers hashString else []

    //        List.append periodStrings hashStrings

    let matchesNumberExactly (cs: char array) (num: int) : bool =
        if (Array.isEmpty cs) then
            false
        else
            cs
            |> Array.skipWhile ((=) '.')
            |> Array.takeWhile ((=) '#')
            |> Array.length = num

    let pruneStringsAndNumbers
        (strings: char array array)
        (numbers as (n :: ns): int list)
        : (char array array) * (int list) =

        let hej =
            strings
            |> Array.map (fun cs ->
                let restLength =
                    cs
                    |> Array.skipWhile ((=) '.')
                    |> Array.skipWhile ((=) '#')
                    |> Array.skipWhile ((=) '.')
                    |> Array.length

                let takens = cs |> Array.take (cs.Length - restLength)

                let realTakens =
                    if (Array.isEmpty takens |> not)
                       && Array.last takens = '.' then
                        takens
                    else
                        [||]
                //match Array.tryFindIndex (fun c -> c= '?') cs with
                //| None -> [||]
                //| Some i ->  cs |> Array.take (i) |> Array.

                (realTakens, (cs |> Array.skip (realTakens.Length))))

        if hej
           |> Array.forall (fun (a, _) -> matchesNumberExactly a n) then

            (hej |> Array.map snd, ns)
        else
            (strings, numbers)

    let rec permuteArrangement2 (numbers: int list) (inputStrings: (char array) array) : string list =
        match Array.tryFindIndex (fun c -> c = '?') inputStrings[0] with
        | None ->
            inputStrings
            |> Array.map (fun cs -> String.Concat cs)
            |> List.ofArray
        | Some i ->
            let periodStrings = inputStrings |> Array.map (Array.updateAt i '.')
            let hashStrings = inputStrings |> Array.map (Array.updateAt i '#')

            let filteredPeriodStrings =
                periodStrings
                |> Array.filter (fun a -> validateStringAdvanced2 numbers (a |> Array.toList))

            let filteredHashStrings =
                hashStrings
                |> Array.filter (fun a -> validateStringAdvanced2 numbers (a |> Array.toList))

            let totalStrings = Array.append filteredPeriodStrings filteredHashStrings

            //let (totalStrings', numbers') = pruneStringsAndNumbers totalStrings numbers

            permuteArrangement2 numbers totalStrings

    //let periodStrings = if (numberOfUnknowns <= hashesLeftToUse) then [] else if validateStringAdvanced numbers periodString then permuteArrangement2 numberOfHashes (numberOfUnknowns-1) numbers periodString else []
    //let hashStrings = if numberOfHashes >= numSum then [] else if validateStringAdvanced numbers hashString then permuteArrangement2 (numberOfHashes+1) (numberOfUnknowns-1) numbers hashString else []

    //List.append periodStrings hashStrings

    let rec permuteArrangement3 (inputStrings: LineContainer array) : int =
        if inputStrings
           |> Array.forall (fun (_, ns) -> List.isEmpty ns) then
            inputStrings |> Array.length
        else

            let beforePrune =
                inputStrings
                |> Array.map (fun (l, ns) ->
                    match Array.tryFindIndex (fun c -> c = '?') l with
                    | None -> ([| (l, ns) |])
                    | Some i ->
                        let combined =
                            [| ((Array.updateAt (i) '.' l), ns)
                               ((Array.updateAt (i) '#' l), ns) |]

                        combined


                )
                |> Array.concat

            let afterPrune = beforePrune |> Array.map prune

            //let comma = ", "
            //let finishedS = "finished"
            //let unfinishedS = "unfinished"
            //let BeforePruneDebug = beforePrune |> Array.map (fun (cs,is) -> $"{cs |> String.Concat} {is |> List.map (fun i -> i.ToString() + comma) |> String.Concat}")
            //let AfterPruneDebug = afterPrune |> Array.map (fun o ->
            //    match o with
            //    | None -> "None"
            //    | Some ((cs,is),finished) -> $"{if finished then finishedS else unfinishedS} {cs |> String.Concat} {is |> List.map (fun i -> i.ToString() + comma) |> String.Concat}")

            let newStrings = afterPrune |> Array.choose id

            if Array.forall snd newStrings then
                inputStrings |> Array.length
            else

                //let filteredPeriodStrings = periodStrings |> Array.Parallel.filter (fun a -> validateStringAdvanced2 numbers (a |> Array.toList))
                //let filteredHashStrings = hashStrings |> Array.Parallel.filter (fun a -> validateStringAdvanced2 numbers (a |> Array.toList))

                //let totalStrings = Array.append filteredPeriodStrings filteredHashStrings


                permuteArrangement3 (newStrings |> Array.map fst)



    //let rec permuteArrangement3 (inputStrings: (StringRep*(int list)) array) : string list =
    //    match Array.tryFindIndex (fun c -> c= '?') (fst inputStrings[0]) with
    //    | None -> inputStrings |> Array.map (fun cs -> String.Concat cs) |> List.ofArray
    //    | Some i ->
    //        let periodStrings = inputStrings |> Array.map (Array.updateAt i '.')
    //        let hashStrings = inputStrings |> Array.map (Array.updateAt i '#')

    //        let filteredPeriodStrings = periodStrings |> Array.filter (fun a -> validateStringAdvanced2 numbers (a |> Array.toList))
    //        let filteredHashStrings = hashStrings |> Array.filter (fun a -> validateStringAdvanced2 numbers (a |> Array.toList))

    //        let totalStrings = Array.append filteredPeriodStrings filteredHashStrings

    //        //let (totalStrings', numbers') = pruneStringsAndNumbers totalStrings numbers

    //        permuteArrangement3 numbers totalStrings

    //        //let periodStrings = if (numberOfUnknowns <= hashesLeftToUse) then [] else if validateStringAdvanced numbers periodString then permuteArrangement2 numberOfHashes (numberOfUnknowns-1) numbers periodString else []
    //        //let hashStrings = if numberOfHashes >= numSum then [] else if validateStringAdvanced numbers hashString then permuteArrangement2 (numberOfHashes+1) (numberOfUnknowns-1) numbers hashString else []

    //        //List.append periodStrings hashStrings

    let validArrangements ((input, numbers): Input) : int =
        let questionmarks = input |> Seq.filter ((=) '?') |> Seq.length
        let existingHashes = input |> Seq.filter ((=) '#') |> Seq.length
        let totalKnownHashes = numbers |> Array.sum
        let missingHashes = totalKnownHashes - existingHashes

        //let baseLine = (List.replicate (questionmarks-missingHashes) '.') @ (List.replicate missingHashes '#')

        //let permuted = permuteArrangement existingHashes questionmarks numbers (input |> Seq.toArray) |> List.toArray
        let numbers = numbers |> Array.toList

        let permuted =
            permuteArrangement2 numbers (input |> Seq.toArray |> Array.singleton)
            |> List.toArray

        //let filtered = permuted |> Array.filter (fun x -> x |> Seq.filter ((=) '#') |> Seq.length = totalKnownHashes)

        //if permuted.Length <> filtered.Length then
        //    printf ""

        permuted |> Array.length

    let validArrangements2 ((input, numbers): Input) : int =
        let numbers = numbers |> Array.toList
        let permuted = permuteArrangement3 [| (("." + input) |> Array.ofSeq, numbers) |]

        permuted

    let part1 (input: string) : string =
        let res = prune ([| '.'; '?'; '?' |], [])

        let res2 = permuteArrangement3 [| ([| '.'; '?'; '?'; '?'; '?' |], [ 1 ]) |]


        let hej = input |> lines |> Array.map parseInputLine

        let valids = hej |> Array.map validArrangements2

        valids |> Array.sum |> string

    let unfoldRecord ((string, numbers): Input) : Input =
        let newString = string |> Array.ofSeq

        ([| newString
            [| '?' |]
            newString
            [| '?' |]
            newString
            [| '?' |]
            newString
            [| '?' |]
            newString |]
         |> Array.concat
         |> String.Concat,
         numbers |> Array.replicate 5 |> Array.concat)

    let part2 (input: string) : string =
        let hej =
            input
            |> lines
            |> Array.map parseInputLine
            |> Array.map unfoldRecord

        printfn $"{hej.Length} inputs"

        let timer = System.Diagnostics.Stopwatch()

        timer.Start()

        let valids =
            hej
            |> Array.mapi (fun i x ->
                let timer = System.Diagnostics.Stopwatch()

                timer.Start()

                let ans = validArrangements2 x

                let elapsed = timer.Elapsed

                printfn $"at {i}. Elapsed time: {elapsed}"

                ans)

        valids |> Array.sum |> string
