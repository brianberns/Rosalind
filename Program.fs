open System
open System.IO

let problemDna dataset =
    let lookup =
        dataset
            |> Seq.groupBy id
            |> Seq.map (fun (key, nts) ->
                key, nts |> Seq.length)
            |> Map.ofSeq
    sprintf "%A %A %A %A"
        lookup.['A']
        lookup.['C']
        lookup.['G']
        lookup.['T']

let problemRna (dataset : string) =
    dataset.Replace("T", "U")

let reverseComplement str =
    let complement symbol =
        match symbol with
            | 'A' -> 'T'
            | 'T' -> 'A'
            | 'G' -> 'C'
            | 'C' -> 'G'
            | _ -> failwith "Unexpected"
    let chars =
        str
            |> Seq.toArray
            |> Array.rev
            |> Seq.map complement
            |> Seq.toArray
    new string(chars)

let problemRevc dataset =
    reverseComplement dataset

let readFile (path : string) =
    use rdr = new StreamReader(path)
    let lines =
        seq {
            let more = ref true
            while !more do
                let line = rdr.ReadLine()
                if isNull line then
                    more := false
                else
                    yield line
        }
    lines |> Seq.toArray

type FastaLabel = string
type FastaValue = string
type FastaPair = (FastaLabel * FastaValue)

let readFasta path : seq<FastaPair> =
    let lines = readFile path
    let numberedLines = lines |> Seq.mapi (fun i line -> i, line)
    let groups =
        numberedLines
            |> Seq.groupBy (fun (i, line) -> line.StartsWith(">"))
            |> Map.ofSeq
    let labelLines = groups.[true]
    let contentLines = groups.[false]
    Seq.append labelLines [ Int32.MaxValue, ">" ]
        |> Seq.pairwise
        |> Seq.map (fun ((iFrom, label), (iTo, _)) ->
            let content =
                contentLines
                    |> Seq.where (fun (i, _) -> (i > iFrom) && (i < iTo))
                    |> Seq.map snd
                    |> Seq.concat
                    |> Seq.toArray
                    |> String
            label.Substring(1), content)

let problemGc path =
    let gcContent str =
        let count =
            str
                |> Seq.where (fun sym -> sym = 'C' || sym = 'G')
                |> Seq.length
        float (100 * count) / float (str |> Seq.length)
    let id, pct =
        readFasta path
            |> Seq.map (fun (label, str) ->
                label, gcContent str)
            |> Seq.maxBy snd
    printfn "%s" id
    printfn "%A" pct

let problemSubs path =
    let lines = readFile path
    let str = lines.[0]
    let pattern = lines.[1]
    let rec indexesOf (str : string) offset =
        seq {
            let index = str.IndexOf(pattern)
            if index <> -1 then
                let position = index + offset + 1
                yield position
                yield! indexesOf (str.Substring(index + 1)) position
        }
    let positions = (indexesOf str 0 |> Seq.toArray)
    for position in positions do
        printf "%d " position
    printfn ""

let tail len (str : string) =
    str.Substring(str.Length - len)

let isOverlap (left : string) (right : string) len =
    right.StartsWith(left |> tail len)

let problemGrph path =
    let substrLen = 3
    let fasta = readFasta path
    for labelLeft, strLeft in fasta do
        for labelRight, strRight in fasta do
            if labelLeft <> labelRight && strLeft.Length >= substrLen then
                if isOverlap strLeft strRight substrLen then
                    printfn "%s %s" labelLeft labelRight

let cross xs ys =
    seq {
        for x in xs do
            for y in ys do
                yield x, y
    }

type OverlapAvailable =
    {
        Value : FastaValue
        LeftLength : int
        RightLength : int
    }

let problemLong path =
    let fasta = readFasta path
    let findOverlap (left : string) (right : string) minLen =
        let maxLen = min left.Length right.Length
        [maxLen .. -1 .. minLen]
            |> Seq.tryFind (fun len -> isOverlap left right len)
            |> Option.map (fun len -> String.Concat(left, right.Substring(len)))
    let rec merge (fastaMap : Map<FastaLabel, OverlapAvailable>) =
        let labelLeft, labelRight, overlap =
            cross fastaMap fastaMap
                |> Seq.where (fun (KeyValue(labelLeft, _), KeyValue(labelRight, _)) -> labelLeft <> labelRight)
                |> Seq.map (fun (KeyValue(labelLeft, availLeft), KeyValue(labelRight, availRight)) ->
                    let minLen = max availLeft.RightLength availRight.LeftLength
                    labelLeft, labelRight, findOverlap availLeft.Value availRight.Value minLen)
                |> Seq.where (fun (_, _, overlapOpt) -> overlapOpt |> Option.isSome)
                |> Seq.map (fun (labelLeft, labelRight, overlapOpt) -> labelLeft, labelRight, overlapOpt.Value)
                |> Seq.head
        let newFastaMap =
            let newLabel = (labelLeft + "+" + labelRight)
            let newAvail =
                {
                    Value = overlap
                    LeftLength = fastaMap.[labelLeft].LeftLength
                    RightLength = fastaMap.[labelRight].RightLength
                }
            fastaMap
                |> Map.remove labelLeft
                |> Map.remove labelRight
                |> Map.add newLabel newAvail
        match newFastaMap |> Seq.length with
            | 1 -> (newFastaMap |> Seq.head).Value.Value
            | 0 -> failwith "Empty"
            | _ -> merge newFastaMap
    let fastaMap =
        fasta
            |> Seq.map (fun (label, str) ->
                let len = str.Length / 2 + 1
                label, { Value = str; LeftLength = len; RightLength = len})
            |> Map.ofSeq
    printfn "%s" (merge fastaMap)

let hammingDistance str0 str1 =
    Seq.zip str0 str1
        |> Seq.where (fun (c0, c1) -> c0 <> c1)
        |> Seq.length

let problemHamm path =
    let lines = readFile path
    let line0 = lines.[0]
    let line1 = lines.[1]
    let hamm = hammingDistance line0 line1
    printfn "%A" hamm

let problemCorr path =
    let strs = readFasta path |> Seq.map snd
    let groups =
        strs
            |> Seq.groupBy (fun str ->
                let revc = reverseComplement str
                let array =
                    [str; revc]
                        |> Seq.sort
                        |> Seq.toArray
                array.[0], array.[1])
            |> Seq.groupBy (fun (key, strs) -> strs |> Seq.length > 1)
            |> Map.ofSeq
    let goodStrs =
        groups.[true]
            |> Seq.collect (fun ((str, revc), _) -> [str; revc])
            |> Seq.toArray
    for badStr in groups.[false] |> Seq.map (snd >> Seq.head) do
        let goodStr =
            goodStrs
                |> Seq.where (fun goodStr -> hammingDistance goodStr badStr = 1)
                |> Seq.exactlyOne
        printfn "%s->%s" badStr goodStr

let problemPerm n =
    let rec perm (items : Set<'item>) : seq<seq<'item>> =
        match items |> Set.count with
            | 0 -> Seq.empty
            | 1 -> seq { yield items |> Set.toSeq }
            | _ ->
                items
                    |> Seq.collect (fun item ->
                        let subItems = items |> Set.remove item
                        perm subItems
                            |> Seq.map (fun subItemPerm ->
                                seq {
                                    yield item
                                    yield! subItemPerm
                                }))
    let perms = perm ([1 .. n] |> Set.ofSeq) |> Seq.toArray
    printfn "%A" (perms.Length)
    for items in perms do
        for item in items do
            printf "%A " item
        printfn ""

let codonMap =
    [|
        "UUU", Some "F"
        "UUC", Some "F"
        "UUA", Some "L"
        "UUG", Some "L"
        "UCU", Some "S"
        "UCC", Some "S"
        "UCA", Some "S"
        "UCG", Some "S"
        "UAU", Some "Y"
        "UAC", Some "Y"
        "UAA", None
        "UAG", None
        "UGU", Some "C"
        "UGC", Some "C"
        "UGA", None
        "UGG", Some "W"
        "CUU", Some "L"
        "CUC", Some "L"
        "CUA", Some "L"
        "CUG", Some "L"
        "CCU", Some "P"
        "CCC", Some "P"
        "CCA", Some "P"
        "CCG", Some "P"
        "CAU", Some "H"
        "CAC", Some "H"
        "CAA", Some "Q"
        "CAG", Some "Q"
        "CGU", Some "R"
        "CGC", Some "R"
        "CGA", Some "R"
        "CGG", Some "R"
        "AUU", Some "I"
        "AUC", Some "I"
        "AUA", Some "I"
        "AUG", Some "M"
        "ACU", Some "T"
        "ACC", Some "T"
        "ACA", Some "T"
        "ACG", Some "T"
        "AAU", Some "N"
        "AAC", Some "N"
        "AAA", Some "K"
        "AAG", Some "K"
        "AGU", Some "S"
        "AGC", Some "S"
        "AGA", Some "R"
        "AGG", Some "R"
        "GUU", Some "V"
        "GUC", Some "V"
        "GUA", Some "V"
        "GUG", Some "V"
        "GCU", Some "A"
        "GCC", Some "A"
        "GCA", Some "A"
        "GCG", Some "A"
        "GAU", Some "D"
        "GAC", Some "D"
        "GAA", Some "E"
        "GAG", Some "E"
        "GGU", Some "G"
        "GGC", Some "G"
        "GGA", Some "G"
        "GGG", Some "G"
    |] |> Map.ofSeq

let translate (rna : string) =
    let codons =
        rna
            |> Seq.mapi (fun i sym -> i, sym)
            |> Seq.groupBy (fun (i, sym) -> i / 3)
            |> Seq.map (fun (_, pairs) ->
                let syms = pairs |> Seq.map snd |> Seq.toArray
                new string(syms))
            |> Seq.toArray
    assert(codons |> Seq.head = "AUG")
    let aas =
        codons
            |> Seq.map (fun codon -> codonMap.[codon])
            |> Seq.takeWhile (fun aaOpt -> aaOpt.IsSome)
            |> Seq.map (fun aaOpt -> aaOpt.Value)
    printfn "%A" (aas |> String.concat "")

let problemIprb k m n =
    let t = k + m + n
    let div x y = (float x) / (float y)
    let first_AA = div k t
    let first_Aa =
        let f = div k t
        let second_AA = f * div k (t-1)
        let second_Aa = f * div (m-1) (t-1) * 0.75
        let second_aa = f * div n (t-1) * 0.5
        second_AA + second_Aa + second_aa
    let first_aa =
        let f = div n t
        let second_AA = f * div k (t - 1)
        let second_Aa = f * div m (t - 1) * 0.5
        let second_aa = 0.0
        second_AA + second_Aa + second_aa
    first_AA + first_Aa + first_aa

type Node = int
type Graph =
    {
        Size : int
        Edges : List<Node * Node>
    }

let createGraph size =
    {
        Size = size
        Edges = []
    }

let addEdge nodeA nodeB graph =
    {
        graph with
            Edges = (nodeA, nodeB) :: graph.Edges
    }

let closure node graph =
    let rec loop node graph nodesFound =
        assert (nodesFound |> Set.contains node)
        let nodesToAdd =
            graph.Edges
                |> Seq.choose (fun (nodeA, nodeB) ->
                    if node = nodeA && not (nodesFound |> Set.contains nodeB) then
                        Some nodeB
                    else if node = nodeB && not (nodesFound |> Set.contains nodeA) then
                        Some nodeA
                    else
                        None)
                |> Set.ofSeq
        if nodesToAdd |> Seq.isEmpty then
            nodesFound
        else
            let nodesFound = Set.union nodesFound nodesToAdd
            (nodesFound, nodesToAdd)
                ||> Seq.fold (fun acc nodeToAdd -> loop nodeToAdd graph acc)
    loop node graph (Set.empty |> Set.add node)

let countSubgraphs graph =
    let rec loop graph removedNodes nSubgraphs =
        let nodeOpt =
            [1 .. graph.Size]
                |> Seq.tryFind (fun node -> not (removedNodes |> Set.contains node))
        match nodeOpt with
            | Some node ->
                let nodesToRemove = graph |> closure node
                let removedNodes = Set.union removedNodes nodesToRemove
                loop graph removedNodes (nSubgraphs + 1)
            | None -> nSubgraphs
    loop graph Set.empty 0

let problemTree path =
    let lines = readFile path
    let graph = createGraph (Int32.Parse lines.[0])
    let graph =
        (graph, lines |> Seq.skip 1)
            ||> Seq.fold (fun acc line ->
                let nodes =
                    line.Split(' ')
                        |> Seq.map Int32.Parse
                        |> Seq.toArray
                acc |> addEdge nodes.[0] nodes.[1])
    let nSubgraphs = (graph |> countSubgraphs)
    printfn "%A" (nSubgraphs - 1)

let problemFib n k =
    let rec fib i =
        match i with
            | 1 -> 1L
            | 2 -> 1L
            | _ -> (fib (i-1)) + k * (fib (i-2))
    fib n

let problemMrna path =
    let revMap =
        codonMap
            |> Seq.map (fun (KeyValue(codon, aaOpt)) -> aaOpt, codon)
            |> Seq.groupBy fst
            |> Seq.map (fun (aaOpt, pairs) -> aaOpt, bigint (pairs |> Seq.length))
            |> Map.ofSeq
    let protein = File.ReadAllText(path)
    let count =
        (revMap.[None], protein)
            ||> Seq.fold (fun acc aa -> acc * revMap.[Some (new string(aa, 1))])
    printfn "%A" (count % 1000000I)

let problemSset (n : int) =
    (2I ** n) % (1000000I)

let asCsv values =
    values
        |> Seq.map (fun value -> value.ToString())
        |> String.concat ", "

let problemSeto path =
    let lines = readFile path
    let n = Int32.Parse lines.[0]
    let parseSet (line : string) =
        line.Split([| '{'; ','; ' '; '}' |], StringSplitOptions.RemoveEmptyEntries)
            |> Seq.map Int32.Parse
            |> Set.ofSeq
    let aSet = lines.[1] |> parseSet
    let bSet = lines.[2] |> parseSet
    let printSeq items =
        printfn "{%s}" (items |> asCsv)
    printSeq (Set.union aSet bSet)
    printSeq (Set.intersect aSet bSet)
    printSeq (Set.difference aSet bSet)
    printSeq (Set.difference bSet aSet)
    let full = [1..n] |> Set.ofSeq
    printSeq (Set.difference full aSet)
    printSeq (Set.difference full bSet)

let problemCons path =
    let lines =
        readFasta path
            |> Seq.map snd
            |> Seq.toArray
    let n = lines.[0].Length
    let profile =
        ['A'; 'C'; 'G'; 'T']
            |> Seq.map (fun sym ->
                sym, [0 .. n-1]
                    |> Seq.map (fun i ->
                        lines
                            |> Seq.where (fun line -> line.[i] = sym)
                            |> Seq.length)
                    |> Seq.toArray)
            |> Map.ofSeq
    let concensus =
        [0 .. n-1]
            |> Seq.map (fun i ->
                profile
                    |> Seq.map (fun (KeyValue(sym, counts)) -> sym, counts.[i])
                    |> Seq.maxBy snd
                    |> fst)
    let separate sep values =
        values
            |> Seq.map (fun value -> value.ToString())
            |> String.concat sep
    printfn "%s" (separate "" concensus)
    for (KeyValue(sym, counts)) in profile do
        printfn "%c: %s" sym (separate " " counts)

let problemProb path =
    let lines = readFile path
    let str = lines.[0]
    let gcProbs =
        lines.[1].Split([|' '|])
            |> Seq.map Double.Parse
            |> Seq.toArray
    for gcProb in gcProbs do
        let prob =
            (1.0, str)
                ||> Seq.fold (fun acc sym ->
                    let prob =
                        match sym with
                            | 'C'
                            | 'G' -> gcProb / 2.0
                            | 'A'
                            | 'T' -> (1.0 - gcProb) / 2.0
                            | _ -> failwith "Unexpected"
                    prob * acc)
        printf "%A " (Math.Log10 prob)
    printfn ""

let problemKmp path =
    let line = readFasta path |> Seq.exactlyOne |> snd
    let (result : int[]) = Array.create (line.Length + 1) -1
    for idx = 1 to line.Length do
        let mutable pos = result.[idx - 1]
        while pos <> -1 && line.[pos] <> line.[idx - 1] do
            pos <- result.[pos]
        result.[idx] <- pos + 1
    for n in result |> Seq.skip 1 do
        printf "%A " n
    printfn ""

[<EntryPoint>]
let main argv =
    printfn "%s" (problemDna "AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC")
    printfn "%s" (problemRna "GATGGAACTTGACTACGTAAATT")
    printfn "%s" (problemRevc "AAAACCCGGT")
    problemGc "problemGc.txt"
    problemSubs "problemSubs.txt"
    problemGrph "problemGrph.txt"
    problemLong "problemLong.txt"
    problemHamm "problemHamm.txt"
    problemCorr "problemCorr.txt"
    problemPerm 3
    translate "AUGGCCAUGGCGCCCAGAACUGAGAUCAAUAGUACCCGUAUUAACGGGUGA"
    printfn "%A" (problemIprb 29 29 25)
    problemTree "problemTree.txt"
    printfn "%A" (problemFib 32 5L)
    problemMrna "problemMrna.txt"
    printfn "%A" (problemSset 871)
    problemSeto "problemSeto.txt"
    problemCons "problemCons.txt"
    problemProb "problemProb.txt"
    problemKmp "problemKmp.txt"
    0
