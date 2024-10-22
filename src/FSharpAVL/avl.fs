module AVL

open System.Collections.Generic
open System.Collections

type private Vertex<'K, 'V> =
    | Node of int * int * 'K * 'V * Vertex<'K, 'V> * Vertex<'K, 'V> // count, height, key, value, l-child, r-child
    | Nil

let private height node =
    match node with
    | Node(_, h, _, _, _, _) -> h
    | Nil -> 0

let private count node =
    match node with
    | Node(c, _, _, _, _, _) -> c
    | Nil -> 0

let private diff node =
    match node with
    | Node(_, _, _, _, l, r) -> height l - height r
    | Nil -> 0

let private createVertex k v l r =
    Node(1 + count l + count r, 1 + max (height l) (height r), k, v, l, r)

let private balance node =
    let rotateLeft node =
        match node with
        | Node(_, _, k, v, l, Node(_, _, rk, rv, rl, rr)) -> createVertex rk rv (createVertex k v l rl) rr
        | v -> v

    let rotateRight node =
        match node with
        | Node(_, _, k, v, Node(_, _, lk, lv, ll, lr), r) -> createVertex lk lv ll (createVertex k v lr r)
        | v -> v

    let bigRotateLeft node =
        match node with
        | Node(_, _, k, v, l, r) -> //
            rotateLeft (createVertex k v l (rotateRight r))
        | v -> v

    let bigRotateRight node =
        match node with
        | Node(_, _, k, v, l, r) -> //
            rotateRight (createVertex k v (rotateLeft l) r)
        | v -> v

    match node with
    | Node(_, _, _, _, l, r) when diff node <= -2 ->
        if diff r = -1 then //
            rotateLeft node
        else
            bigRotateLeft node
    | Node(_, _, _, _, l, r) when diff node >= 2 ->
        if diff l = 1 then //
            rotateRight node
        else
            bigRotateRight node
    | v -> v

let rec private insert k v node =
    match node with
    | Node(_, _, k', v', l', r') when k' = k -> createVertex k v l' r'
    | Node(_, _, k', v', l', r') ->
        let l, r = if k < k' then insert k v l', r' else l', insert k v r'
        createVertex k' v' l r |> balance
    | Nil -> createVertex k v Nil Nil

let rec private tryGet node k =
    match node with
    | Node(_, _, k', v', _, _) when k = k' -> Some v'
    | Node(_, _, k', _, l', _) when k < k' -> tryGet l' k
    | Node(_, _, k', _, _, r') when k > k' -> tryGet r' k
    | _ -> None

let rec private treeSeq tree =
    seq {
        match tree with
        | Node(_, _, k, v, l, r) ->
            yield! treeSeq l
            yield k, v
            yield! treeSeq r
        | Nil -> ()
    }

let rec private treeSeqBack tree =
    seq {
        match tree with
        | Node(_, _, k, v, l, r) ->
            yield! treeSeqBack r
            yield k, v
            yield! treeSeqBack l
        | Nil -> ()
    }

let private dumpTree node =
    let rec dumpTreeHelp node parent x =
        match node with
        | Node(c, h, k, v, l, r) ->
            printfn "%d [label = \"%A:%A\"];" x k v

            match parent with
            | Some p -> printfn "%d -> %d;" p x
            | None -> ()

            let res = dumpTreeHelp l (Some x) (x + 1)
            dumpTreeHelp r (Some x) (res + 1)
        | Nil -> x

    printfn "digraph {"
    ignore <| dumpTreeHelp node None 0
    printfn "}"

let rec private map node f =
    match node with
    | Node(_, _, k, v, l, r) -> createVertex k (f v) (map l f) (map r f)
    | Nil -> Nil

let rec private maxDiff node =
    match node with
    | Node(_, _, k, v, l, r) -> max (diff node) (max (maxDiff l) (maxDiff r))
    | Nil -> 0


type AVLTree<'Key, 'Value when 'Key: comparison> private (root: Vertex<'Key, 'Value>) =
    public new() = AVLTree(Nil)

    member _.Height = height root

    member _.TryGet = tryGet root

    member _.Add (k: 'Key) v = AVLTree(insert k v root)

    member _.Dump = dumpTree root

    member _.Map f = AVLTree(map root f)

    member _.BackSeq = treeSeqBack root

    member _.MaxDiff = maxDiff root

    member this.Item
        with get (k: 'Key): 'Value = tryGet root k |> Option.get

    interface IReadOnlyDictionary<'Key, 'Value> with
        member this.Item
            with get (k: 'Key): 'Value = this.[k]

        member this.Keys: IEnumerable<'Key> =
            (this :> seq<'Key * 'Value>) |> Seq.map (fun (k, v) -> k)

        member this.Values: IEnumerable<'Value> =
            (this :> seq<'Key * 'Value>) |> Seq.map (fun (k, v) -> v)

        member this.ContainsKey(k: 'Key) : bool = tryGet root k |> Option.isSome

        member this.TryGetValue(key: 'Key, value: byref<'Value>) : bool =
            match tryGet root key with
            | Some v ->
                value <- v
                true
            | None -> false

        member this.Count: int = count root

        member this.GetEnumerator() =
            (treeSeq root |> Seq.map (fun (k, v) -> KeyValuePair(k, v)) :> IEnumerable<KeyValuePair<'Key, 'Value>>)
                .GetEnumerator()

    interface IEnumerable with
        member _.GetEnumerator() =
            (treeSeq root).GetEnumerator() :> IEnumerator

    interface IEnumerable<'Key * 'Value> with
        member _.GetEnumerator() = (treeSeq root).GetEnumerator()

module AVLTree =
    [<GeneralizableValue>]
    let empty<'K, 'V when 'K: comparison> : AVLTree<'K, 'V> = AVLTree<'K, 'V>()

    let add k v (tree: AVLTree<'K, 'V>) = tree.Add k v

    let map f (tree: AVLTree<'K, 'V>) = tree.Map f

    let ofItems (items: IEnumerable<'K * 'V>) =
        items |> Seq.fold (fun tree (k, v) -> tree |> add k v) empty

    let merge (lhs: AVLTree<'K, 'V>) (rhs: AVLTree<'K, 'V>) =
        lhs |> Seq.fold (fun tree (k, v) -> tree |> add k v) rhs

    let filter pred (tree: AVLTree<'K, 'V>) = tree |> Seq.filter pred |> ofItems

    let fold f state (tree: AVLTree<'K, 'V>) = tree |> Seq.fold f state
    let foldBack f state (tree: AVLTree<'K, 'V>) = tree.BackSeq |> Seq.fold f state

    let tryGet k (tree: AVLTree<'K, 'V>) = tree.TryGet k

    let dump (tree: AVLTree<'K, 'V>) = tree.Dump
