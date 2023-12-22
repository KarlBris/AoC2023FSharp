namespace AoC2023

open Utils

module Day22 =

    type Brick = (int * int * int) * (int * int * int)

    let parseBrick (input: string) : Brick =
        input
        |> split "~"
        |> Array.map (fun brick ->
            brick
            |> commas
            |> Array.map int
            |> (fun is -> (is[0], is[1], is[2])))
        |> twoArrayToTuple

    let lowerBrick (amount: int) (((x1, y1, z1), (x2, y2, z2)): Brick) : Brick =
        ((x1, y1, z1 - amount), (x2, y2, z2 - amount))

    let collides (((ax1, ay1, az1), (ax2, ay2, az2)): Brick) (((bx1, by1, bz1), (bx2, by2, bz2)): Brick) : bool =
        let xOverlap =
            not (
                (max ax1 ax2 < min bx1 bx2)
                || (min ax1 ax2 > max bx1 bx2)
            )

        let yOverlap =
            not (
                (max ay1 ay2 < min by1 by2)
                || (min ay1 ay2 > max by1 by2)
            )

        let zOverlap =
            not (
                (max az1 az2 < min bz1 bz2)
                || (min az1 az2 > max bz1 bz2)
            )

        xOverlap && yOverlap && zOverlap

    let lowestZ (((_, _, z1), (_, _, z2)): Brick) : int = min z1 z2

    let rec settleBrick (fell: bool) (brick: Brick) (settledBricks: Brick list) : ((Brick list) * bool) =
        let newBrick = lowerBrick 1 brick

        let isValidPosition =
            ((lowestZ brick) > 0)
            && (settledBricks
                |> List.exists (collides newBrick)
                |> not)

        if isValidPosition then
            settleBrick true newBrick settledBricks
        else
            (brick :: settledBricks, fell)

    let rec settleBricks
        (fallenCount: int)
        (settledBricks: Brick list)
        (fallingBricks: Brick list)
        : ((Brick list) * int) =
        match fallingBricks with
        | [] -> (settledBricks, fallenCount)
        | b :: bs ->
            let (settledBricks', fell) = (settleBrick false b settledBricks)

            settleBricks
                (if fell then
                     fallenCount + 1
                 else
                     fallenCount)
                settledBricks'
                bs

    let rec isDisposable (settledBricks: Brick list) (currentBrick: Brick) : bool =
        let bricksExceptCurrent = settledBricks |> List.filter ((<>) currentBrick)
        let raisedCurrentBrick = lowerBrick -1 currentBrick

        bricksExceptCurrent
        |> List.filter (collides raisedCurrentBrick)
        |> List.map (fun restingBrick ->
            let restingExceptTheOne =
                bricksExceptCurrent
                |> List.filter ((<>) restingBrick)

            let loweredRestingBrick = lowerBrick 1 restingBrick

            restingExceptTheOne
            |> List.exists (collides loweredRestingBrick))
        |> List.fold (&&) true

    let prepareBricks (input: string) : (Brick list) =
        input
        |> lines
        |> Array.map parseBrick
        |> Array.sortBy lowestZ
        |> List.ofArray
        |> settleBricks 0 []
        |> fst

    let part1 (input: string) : string =
        let settledBricks = prepareBricks input

        settledBricks
        |> List.filter (isDisposable settledBricks)
        |> List.length
        |> string

    let part2 (input: string) : string =
        let settledBricks = prepareBricks input

        settledBricks
        |> List.map (fun brickToRemove ->
            settledBricks
            |> List.sortBy lowestZ
            |> List.filter ((<>) brickToRemove)
            |> settleBricks 0 []
            |> snd)
        |> List.sum
        |> string
