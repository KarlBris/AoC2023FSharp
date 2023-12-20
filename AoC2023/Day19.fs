namespace AoC2023

open Utils
open System
open System.Text.RegularExpressions

module Day19 =

    type Workflow = (string * ((((char * char * int) option) * string) list))

    let parseWorkflow (input: string) : Workflow =
        let regex = Regex("(.+){(.*)}", RegexOptions.Compiled)
        let gs = regex.Match(input).Groups

        let name = gs[1].Value

        let rules = gs[2].Value

        let parsedRules =
            rules
            |> commas
            |> List.ofArray
            |> List.map (fun singleRule ->
                let a = singleRule |> colons

                if a.Length = 1 then
                    (None, a[0])
                else
                    let (condition, destination) = twoArrayToTuple a
                    let category = condition[0]
                    let comparison = condition[1]
                    let value = condition[2..] |> int

                    (Some(category, comparison, value), destination))

        (name, parsedRules)

    let parsePart (input: string) : (int * int * int * int) =
        let length = input.Length

        let vs =
            input.Substring(1, length - 2)
            |> commas
            |> Array.map (fun s -> s.Substring(2) |> int)

        (vs[0], vs[1], vs[2], vs[3])

    let rec execute
        (current: string)
        (workflows: Map<string, ((((char * char * int) option) * string) list)>)
        (part as (pX, pM, pA, pS): (int * int * int * int))
        : char =
        if current = "R" || current = "A" then
            current[0]
        else
            let wFlow = workflows[current]

            let newCurrent =
                wFlow
                |> List.pick (fun (cond, dest) ->
                    match cond with
                    | None -> Some dest
                    | Some (category, comparison, value) ->
                        let compValue =
                            match category with
                            | 'x' -> pX
                            | 'm' -> pM
                            | 'a' -> pA
                            | 's' -> pS

                        let compFunc = if comparison = '<' then (<) else (>)

                        if compFunc compValue value then
                            Some dest
                        else
                            None)

            execute newCurrent workflows part


    let part1 (input: string) : string =
        let (workflowInputs, partInputs) = input |> splitAtDoubleLines |> twoArrayToTuple

        let parsedWorkflows =
            workflowInputs
            |> lines
            |> Array.map parseWorkflow
            |> Map.ofArray

        let parsedParts = partInputs |> lines |> Array.map parsePart

        parsedParts
        |> Array.map (fun p -> (p, execute "in" parsedWorkflows p))
        |> Array.filter (fun (_, res) -> res = 'A')
        |> Array.map (fun ((x, m, a, s), _) -> x + m + a + s)
        |> Array.sum
        |> string

    let rec retracePath (wFlows: Workflow list) (label: string) : string list list =
        if label = "in" then
            [["in"]]
        else
            let matchingWorkflows =
                wFlows
                |> List.filter (fun (_,(rules)) -> 
                    rules |> List.exists (fun (o,l) -> l = label))
                |> List.map (fun (l,_) -> l)

            matchingWorkflows
            |> List.map (fun nl -> nl |> retracePath wFlows |> List.map (fun ps -> label::ps))
            |> List.concat

    let satisfyCondition (satisfy: bool) (condition: (char*char*int) option) (vals: (int list * int list * int list * int list)) : (int list * int list * int list * int list) =
        match condition with
        | None -> vals
        | Some (category, comparison, value) ->
            let negateIfNeedBe = if satisfy then id else not
            let (pXs, pMs, pAs, pSs) = vals
            let compFunc = if comparison = '<' then (<) else (>)
            match category with
            | 'x' -> (pXs |> List.filter (fun v -> compFunc v value |> negateIfNeedBe), pMs, pAs, pSs)
            | 'm' -> (pXs, pMs |> List.filter (fun v -> compFunc v value |> negateIfNeedBe), pAs, pSs)
            | 'a' -> (pXs, pMs, pAs |> List.filter (fun v -> compFunc v value |> negateIfNeedBe), pSs)
            | 's' -> (pXs, pMs, pAs, pSs |> List.filter (fun v -> compFunc v value |> negateIfNeedBe))


    let rec pruneValuesToLabel (rules : (((char * char * int) option) * string) list) (label: string) (vals: (int list * int list * int list * int list)) : (int list * int list * int list * int list) =
        if rules |> List.filter (fun (_,l) -> l = label) |> List.length = 0 then
            vals
        else
            //let testLabel = rules |> List.head |> snd
            

            match rules with
            | r1::r2::rest -> 
                if (snd r1) = (snd r2) then
                    pruneValuesToLabel (r2::rest) label vals
                else
                    let (o,l) = r1

                    let vals' = satisfyCondition (l = label) o vals

                    pruneValuesToLabel (r2::rest) label vals'

            | _ ->
                let ((o,l)::rs) = rules
            
                let vals' = satisfyCondition (l = label) o vals

                pruneValuesToLabel rs label vals'

    let visualizeVals (vals: (int list * int list * int list * int list)) : unit =
            let (pXs, pMs, pAs, pSs) = vals
            printfn $"x={List.min pXs}..{List.max pXs}, m={List.min pMs}..{List.max pMs}, a={List.min pAs}..{List.max pAs}, s={List.min pSs}..{List.max pSs}"

    let rec pathToString (path: string list) : string =
        match path with
        | s1::s2::ss -> $"{s1} -> " + (pathToString (s2::ss))
        | s::[] -> s
        | _ -> ""

    let valueListToString (vals: int list) : string =
        if vals.IsEmpty then
            "-"
        else
            $"{List.min vals}..{List.max vals}"

    let visualizePathsAndVals (path: string list, vals: (int list * int list * int list * int list)) : unit =
            let (pXs, pMs, pAs, pSs) = vals
            printfn $"{path |> pathToString}: x={valueListToString pXs}, m={valueListToString pMs}, a={valueListToString pAs}, s={valueListToString pSs}"

    let rec followPath (wFlows: Workflow array) (vals: (int list * int list * int list * int list)) (path: string list): (int list * int list * int list * int list) =
        match path with
        | l1::l2::ls -> 
            let matchingWorkflow = wFlows |> Array.find (fun (label,_) -> label = l1) |> snd

            let vals' = pruneValuesToLabel matchingWorkflow l2 vals
            //visualizeVals vals'
            followPath wFlows vals' (l2::ls)
        | _ -> vals

    let valuesScore ((a,b,c,d): (int list * int list * int list * int list)) : int64 =
        (a |> List.length |> int64) * (b |> List.length |> int64) * (c |> List.length |> int64) * (d |> List.length |> int64)
        

    let part2 (input: string) : string =
        let (workflowInputs, _) = input |> splitAtDoubleLines |> twoArrayToTuple

        let parsedWorkflows = workflowInputs |> lines |> Array.map parseWorkflow

        // TODO: istället för en path från lnx till A får man lov att ha 2. för varje regel i ett 
        // workflow som matchar målet kommer man ha en path. Varje path får då en lista med regler
        // som ska eller inte ska uppfyllas. när alla reglerna har gåtts igenom returnerar man vals

        let acceptingRules =
            parsedWorkflows
            |> Array.map (fun (lbl, flows) ->
                flows
                |> List.map (fun (o,l) -> 
                    if l = "A" then
                        Some lbl
                    else
                        None)
                |> List.choose id)
            |> List.concat
            |> List.distinct


        let acceptingPaths = 
            acceptingRules
            |> List.map (retracePath (List.ofArray parsedWorkflows))
            |> List.concat
            |> List.map ((fun l -> "A"::l) >> List.rev)

        let acceptedValues = 
            acceptingPaths
            |> List.map (fun p -> (p, (followPath parsedWorkflows ([ 1..4000 ], [ 1..4000 ], [ 1..4000 ], [ 1..4000 ]) p)))

        acceptedValues |> List.iter visualizePathsAndVals

        let scoreSum = acceptedValues |> List.map (snd >> valuesScore) |> List.sum

        scoreSum |> string
        //"167409079868000"
