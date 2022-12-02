#load @"..\Read-data.fsx"

let filePath = @"Day 2\input-data.txt"

//A = X = 1 Rock
//B = Y = 2 Paper
//A = Z = 3 Scissors

let rpsResult turn =
    match turn with
    | "A X" -> 1 + 3
    | "A Y" -> 2 + 6
    | "A Z" -> 3 + 0
    | "B X" -> 1 + 0
    | "B Y" -> 2 + 3
    | "B Z" -> 3 + 6
    | "C X" -> 1 + 6
    | "C Y" -> 2 + 0
    | "C Z" -> 3 + 3
    | _ -> failwith "Invalid input"

(ReadData.readLines filePath)
|> Seq.map (rpsResult)
|> Seq.sum
