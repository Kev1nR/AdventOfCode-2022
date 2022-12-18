#load @"..\Read-data.fsx"

let filePath = @"Day 4\input-data.txt"

type Ranges = {LLow : int; LHi : int; RLow : int; RHi : int}
let anyOverlap r =
    not ((r.LHi < r.RLow ) || (r.RHi < r.LLow))

(ReadData.readLines filePath)
|> Seq.map (fun (s: string) -> s.Split(',') |> Seq.map (fun (s : string) -> s.Split('-')) |> Seq.concat)
|> Seq.map (fun i -> i |> Seq.toArray |> fun i -> {LLow = int i[0]; LHi = int i[1]; RLow = int i[2]; RHi = int i[3]})
|> Seq.map (anyOverlap)
|> Seq.filter (fun s -> s = true)
|> Seq.length