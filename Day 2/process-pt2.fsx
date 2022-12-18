#load @"..\Read-data.fsx"

let filePath = @"Day 2\input-data.txt"

let evalLosingHand hand =
    let handVal =
        if hand = "A" then 0
        elif hand = "B" then 1
        else 2
    ((handVal + 2) % 3) + 1

let evalDrawingHand hand =
    let handVal =
        if hand = "A" then 0
        elif hand = "B" then 1
        else 2
    handVal + 1

let evalWinningHand hand =
    let handVal =
        if hand = "A" then 0
        elif hand = "B" then 1
        else 2
    ((handVal + 1) % 3) + 1

let evalTurn (input : string) =
    let hand, result = input.Split(' ') |> fun t -> t[0], t[1]

    match result with
    | "X" -> 0 + evalLosingHand hand
    | "Y" -> 3 + evalDrawingHand hand
    | "Z" -> 6 + evalWinningHand hand
    | _ -> failwith "Invalid turn: %s" input


(ReadData.readLines filePath)
|> Seq.map (evalTurn)
|> Seq.sum
