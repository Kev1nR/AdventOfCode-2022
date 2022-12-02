#load @"..\Read-data.fsx"

let filePath = @"Day 2\input-data-example.txt"

let (|Rock|Paper|Scissors|) input =
    if input = "A" then Rock
    elif input = "B" then Paper
    elif input = "C" then Scissors
    else failwith "Invalid input"



(ReadData.readLines filePath)
|> Seq.map (fun (turn: string) ->
             match turn.Split(' ')[0] with
             | Rock -> 1
             | Paper -> 2
             | Scissors -> 3)
