#load @"..\Read-data.fsx"

let filePath = @"Day 3\input-data.txt"

let charMap (c: char) =
    if (int c > 64) && (int c) < 91 then (int c) - 64 + 26
    elif (int c > 96) && (int c) < 123 then (int c) - 96
    else failwith "invalid char"

(ReadData.readLines filePath)
|> Seq.map (fun (s: string) -> s |> Seq.distinct)
|> Seq.chunkBySize 3
|> Seq.map (fun arr -> arr |> Seq.concat)
|> Seq.map (fun sc -> sc |> Seq.countBy (id) |> Seq.filter (fun x -> (snd x) = 3) |> Seq.head)
|> Seq.map (fun cc -> charMap (fst cc))
|> Seq.sum
