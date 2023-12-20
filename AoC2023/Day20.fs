namespace AoC2023

open Utils

module Day20 =

    type Pulse =
        | H
        | L

    type Module =
        | Broadcaster of (string * (string list))
        | FlipFlop of (string * (string list) * bool)
        | Conjunction of (string * (string list) * ((string * Pulse) list))

    type State = Module array

    let parseModule (moduleInput: string) : Module =
        let (p1, p2) = moduleInput |> split " -> " |> twoArrayToTuple
        let targets = p2 |> commas |> List.ofArray

        match Seq.head p1 with
        | 'b' -> Broadcaster("broadcaster", targets)
        | '%' -> FlipFlop(p1.Substring 1, targets, false)
        | '&' -> Conjunction(p1.Substring 1, targets, [])
        | _ -> failwith ""

    let getModuleName (mdl: Module) : string =
        match mdl with
        | Broadcaster (s, _) -> s
        | FlipFlop (s, _, _) -> s
        | Conjunction (s, _, _) -> s

    let getModuleDests (mdl: Module) : string list =
        match mdl with
        | Broadcaster (_, ds) -> ds
        | FlipFlop (_, ds, _) -> ds
        | Conjunction (_, ds, _) -> ds

    let initializeConjunctions (modules: State) : State =
        modules
        |> Array.map (fun mdl ->
            match mdl with
            | Conjunction (a, dests, _) ->
                let allSources =
                    modules
                    |> Array.filter (fun mdl -> mdl |> getModuleDests |> List.contains a)
                    |> Array.map getModuleName

                Conjunction(
                    a,
                    dests,
                    allSources
                    |> Array.map (fun n -> (n, L))
                    |> Array.toList
                )
            | _ -> mdl)

    let updateState (updatedModule: Module) (state: State) : State =
        let moduleName = getModuleName updatedModule

        state
        |> Array.map (fun mdl ->
            match mdl with
            | Broadcaster (_, _) -> mdl
            | FlipFlop (name, dests, onState) ->
                if name = moduleName then
                    updatedModule
                else
                    mdl
            | Conjunction (name, _, _) ->
                if name = moduleName then
                    updatedModule
                else
                    mdl)

    let allPulsesHigh (mdl: Module) : bool =
        match mdl with
        | Broadcaster _ -> false
        | FlipFlop _ -> false
        | Conjunction (_, _, memory) -> memory |> List.forall (fun (_, b) -> b = H)

    let incrementPulseCount (pulse: Pulse) ((high, low): int * int) : int * int =
        match pulse with
        | H -> (high + 1, low)
        | L -> (high, low + 1)

    let rec sendSignal
        (sigQueue: (string * Pulse * string) list)
        (counts: (int * int))
        (p2: bool)
        (state: State)
        : State * (int * int) * bool =
        match sigQueue with
        | [] -> (state, counts, p2)
        | (source, pulse, dest) :: ss ->
            let p2' = pulse = L && dest = "rx"

            let counts' = incrementPulseCount pulse counts

            match state
                  |> Array.tryFind (fun m -> getModuleName m = dest)
                with
            | None -> sendSignal ss counts' p2' state
            | Some destinationState ->
                match destinationState with
                | Broadcaster (name, dests) ->
                    let newSignals = dests |> List.map (fun d -> (name, pulse, d))
                    sendSignal (ss @ newSignals) counts' p2' state
                | FlipFlop (name, dests, onState) ->
                    match pulse with
                    | H -> sendSignal ss counts' p2' state
                    | L ->
                        let updatedModule = FlipFlop(name, dests, not onState)
                        let newState = updateState updatedModule state
                        let pulseToSend = if onState then L else H

                        let newSignals =
                            dests
                            |> List.map (fun d -> (name, pulseToSend, d))

                        sendSignal (ss @ newSignals) counts' p2' newState
                | Conjunction (name, dests, inputs) ->
                    let updatedModule =
                        Conjunction(
                            name,
                            dests,
                            inputs
                            |> List.map (fun (a, b) ->
                                if a = source then
                                    (a, pulse)
                                else
                                    (a, b))
                        )

                    let newState = updateState updatedModule state

                    let pulseToSend =
                        if allPulsesHigh updatedModule then
                            L
                        else
                            H

                    let newSignals =
                        dests
                        |> List.map (fun d -> (name, pulseToSend, d))

                    sendSignal (ss @ newSignals) counts' p2' newState

    let rec performPresses (presses: int) (counts: (int * int)) (state: State) : int * int =
        if presses = 0 then
            counts
        else
            let (state', counts', _) =
                sendSignal [ ("button", L, "broadcaster") ] counts false state

            performPresses (presses - 1) counts' state'

    let part1 (input: string) : string =
        let modules = input |> lines |> Array.map parseModule

        let originalState = modules |> initializeConjunctions

        let (high, low) = performPresses 1000 (0, 0) originalState

        (int64 high) * (int64 low) |> string

    let rec countToTarget (presses: int) (state: State) : int =
        // TODO: this takes just way too long; sendSignal needs to be memoized somehow
        let (state', _, p2) = sendSignal [ ("button", L, "broadcaster") ] (0, 0) false state
      
        if presses < 0 then
            printf ""

        if p2 then
            presses
        else
            countToTarget (presses + 1) state'

    let part2 (input: string) : string =
        let modules = input |> lines |> Array.map parseModule

        let originalState = modules |> initializeConjunctions

        let count = countToTarget 1 originalState

        count |> string
