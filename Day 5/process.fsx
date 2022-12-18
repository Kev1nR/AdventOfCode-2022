#load @"..\Read-data.fsx"

let filePath = @"Day 5\input-data-sample.txt"

let buildStacks (stacks : string list[][]) =
    let mutable acc : string list[] = Array.init (stacks |> Array.length) (fun i -> [])
    printfn "acc %A" acc
    for r in 0 .. (stacks |> Array.length) - 1 do
        for c in 0 .. (stacks[r] |> Array.length) - 1 do
            printfn "r %d, c %d" r c
            printfn "stack item %A" (stacks[r][c] |> Seq.head)
            printfn "acc item %A" (acc[0])
            let nextItem = (stacks[r][c] |> Seq.head)
            if nextItem.Length > 0 then acc[c] <- nextItem::(acc[c])
    acc

let stacks = (ReadData.readLines filePath)
                          |> Seq.filter (fun s -> s.Contains('['))
                          |> Seq.map (fun s -> s.Replace("   ", "[]").Replace("[","").Replace("]",""))
                          |> Seq.map (fun s -> s.Split(' ') |> Array.toList)
                          |> Seq.map (fun c -> c |> Seq.map (fun c' -> [c']))
                          |> Seq.map (fun s -> s |> Seq.toArray)
                          |> Seq.toArray

let instructions =
    (ReadData.readLines filePath)
    |> Seq.filter (fun s -> s.StartsWith("move"))
    |> Seq.map (fun s ->
                    let parsed = s.Split(' ')
                    parsed[1], parsed[3], parsed[5])

let restack stacks instruction =
    let cnt, src, dst = instruction
    for c in 0 .. cnt -1 do
        let crate = stacks[src-1] |> Seq.head
