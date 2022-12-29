#load @"..\Read-data.fsx"

let filePath = @"Day 5\input-data.txt"

let buildStacks numStacks (stacks : string list[][]) =
    let mutable acc : string list[] = Array.init numStacks (fun i -> [])
    // printfn "acc %A" acc
    for r in 0 .. (stacks |> Array.length) - 1 do
        for c in 0 .. (stacks[r] |> Array.length) - 1 do
            // printfn "r %d, c %d" r c
            // printfn "stack item %A" (stacks[r][c] |> Seq.head)
            // printfn "acc item %A" (acc[0])
            let nextItem = (stacks[r][c] |> Seq.head)
            // printfn "next item %A" nextItem
            if nextItem.Length > 0 then acc[c] <- nextItem::(acc[c])
    acc

let stacks = (ReadData.readLines filePath)
//                          |> Seq.take 2
                          |> Seq.filter (fun s -> s.Contains('['))
                          |> Seq.map (fun s -> s.Replace("    ", " "))
                          |> Seq.map (fun s -> s.Split(" ") |> Array.map (fun s -> s.Replace(" [", "").Replace("[", "").Replace("]","")))

                        //   |> Seq.map (fun s -> s.Replace("   ", "[]").Replace("[","").Replace("]",""))
                        //   |> Seq.map (fun s -> s.Split(' ') |> Array.toList)
                          |> Seq.map (fun c -> c |> Seq.map (fun c' -> [c']))
                          |> Seq.rev
                          |> Seq.map (fun s -> s |> Seq.toArray)
                          |> Seq.toArray

let numStacks =
    (ReadData.readLines filePath)
    |> Seq.filter (fun s -> s.StartsWith(" 1"))
    |> Seq.take 1
    |> Seq.map (fun s -> s.Split(' ')
                        |> Array.filter (fun c -> c <> ""))
    |> Seq.head
    |> Array.length

let instructions =
    (ReadData.readLines filePath)
    |> Seq.filter (fun s -> s.StartsWith("move"))
    |> Seq.map (fun s ->
                    let parsed = s.Split(' ')
                    int parsed[1], int parsed[3], int parsed[5])

let restack (stacks : string list[]) (instruction : int * int * int) =
    let cnt, src, dst = instruction
    for c in 0 .. cnt - 1 do
        let crate = stacks[src-1] |> Seq.head
        // printfn "crate %A" crate
        // printfn "Before stacks[%d] %A " (dst - 1) stacks[dst - 1]
        stacks[src - 1] <- stacks[src-1] |> List.tail
        stacks[dst - 1] <- crate::stacks[dst - 1]
        // printfn "After stacks[%d] %A " (dst - 1) stacks[dst - 1]

    stacks

let restack2 (stacks : string list[]) (instruction : int * int * int) =
    let cnt, src, dst = instruction
    //for c in 0 .. cnt - 1 do
    let crates = stacks[src-1] |> Seq.take cnt |> Seq.toList
    // printfn "crate %A" crate
    // printfn "Before stacks[%d] %A " (dst - 1) stacks[dst - 1]
    stacks[src - 1] <- stacks[src-1] |> List.skip cnt
    stacks[dst - 1] <- crates@stacks[dst - 1]
    // printfn "After stacks[%d] %A " (dst - 1) stacks[dst - 1]

    stacks

let restackx = restack (buildStacks numStacks stacks)

let restacked = instructions |> Seq.fold (fun acc n -> restack2 acc n) (buildStacks numStacks stacks)

let solution restacked = restacked |> Array.fold (fun acc n -> acc + (n |> Seq.head)) ""