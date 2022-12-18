#load @"..\Read-data.fsx"

let filePath = @"Day 3\input-data.txt"

let charMap (c: char) =
    if (int c > 64) && (int c) < 91 then (int c) - 64 + 26
    elif (int c > 96) && (int c) < 123 then (int c) - 96
    else failwith "invalid char"

(ReadData.readLines filePath)
|> Seq.map (fun (s : string) ->
    seq { yield s.Substring(0, s.Length/2) |> Seq.distinct; yield s.Substring(s.Length/2) |> Seq.distinct })
|> Seq.map (fun s -> s
                    |> Seq.concat
                    |> Seq.countBy (id)
                    |> Seq.filter (fun i -> (snd i) > 1))
|> Seq.concat
|> Seq.map (fun cc -> charMap (fst cc))
|> Seq.sum
