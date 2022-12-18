#load @"..\Read-data.fsx"

let filePath = @"Day 4\input-data.txt"

type Ranges = {LLow : int; LHi : int; RLow : int; RHi : int}
let testFullInclusion r =
    (r.LLow <= r.RLow && r.LHi >= r.RHi) || (r.RLow <= r.LLow && r.RHi >= r.LHi)

(ReadData.readLines filePath)
|> Seq.map (fun (s: string) -> s.Split(',') |> Seq.map (fun (s : string) -> s.Split('-')) |> Seq.concat)
|> Seq.map (fun i -> i |> Seq.toArray |> fun i -> {LLow = int i[0]; LHi = int i[1]; RLow = int i[2]; RHi = int i[3]})
|> Seq.map (testFullInclusion)
|> Seq.filter (fun s -> s = true)
|> Seq.length