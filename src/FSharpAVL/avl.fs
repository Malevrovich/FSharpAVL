module AVL

open System.Collections.Generic
open System.Collections

type private Vertex<'K, 'V> =
    | Node of int * 'K * 'V * Vertex<'K, 'V> * Vertex<'K, 'V>
    | Nil

let private height node =
    match node with
    | Node(h, _, _, _, _) -> h
    | Nil -> 0

let private diff node =
    match node with
    | Node(_, _, _, l, r) -> height l - height r
    | Nil -> 0

let private createVertex k v l r =
    Node(1 + max (height l) (height r), k, v, l, r)

let private balance node =
    let rotateLeft node =
        match node with
        | Node(_, k, v, l, Node(_, rk, rv, rl, rr)) -> createVertex rk rv (createVertex k v l rl) rr
        | v -> v

    let rotateRight node =
        match node with
        | Node(_, k, v, Node(_, lk, lv, ll, lr), r) -> createVertex lk lv ll (createVertex k v lr r)
        | v -> v

    let bigRotateLeft node =
        match node with
        | Node(_, k, v, l, r) -> //
            rotateLeft (createVertex k v l (rotateRight r))
        | v -> v

    let bigRotateRight node =
        match node with
        | Node(_, k, v, l, r) -> //
            rotateRight (createVertex k v (rotateLeft l) r)
        | v -> v

    match node with
    | Node(_, _, _, l, r) when diff node <= -2 ->
        if diff r = 1 then //
            bigRotateLeft node
        else
            rotateLeft node
    | Node(_, _, _, l, r) when diff node >= 2 ->
        if diff l = 1 then //
            bigRotateRight node
        else
            rotateRight node
    | v -> v

let rec private insert k v node =
    match node with
    | Node(_, k', v', l', r') when k' = k -> createVertex k v l' r'
    | Node(_, k', v', l', r') ->
        let l, r = if k < k' then insert k v l', r' else l', insert k v r'
        createVertex k' v' l r |> balance
    | Nil -> createVertex k v Nil Nil

let rec private tryGet node k =
    match node with
    | Node(_, k', v', _, _) when k = k' -> Some v'
    | Node(_, k', _, l', _) when k < k' -> tryGet l' k
    | Node(_, k', _, _, r') when k > k' -> tryGet r' k
    | _ -> None

let rec private treeSeq tree =
    seq {
        match tree with
        | Node(_, k, v, l, r) ->
            yield! treeSeq l
            yield! treeSeq r
            yield k, v
        | Nil -> ()
    }

let private dumpTree v =
    let rec dumpDepth h =
        match h with
        | h when h <= 0 -> ()
        | _ ->
            printf "-"
            dumpDepth (h - 1)

    let rec dumpTreeHelp v d s =
        dumpDepth d

        match v with
        | Node(h, k, v, l, r) ->
            printfn "%s %A: %A" s k v
            dumpTreeHelp l (d + 1) "<"
            dumpTreeHelp r (d + 1) ">"

            dumpDepth d
            printfn "* %A" k
        | Nil -> printfn "Nil"


    dumpTreeHelp v 0

module AVLTree =
    type AVLTree<'Key, 'Value when 'Key: comparison> private (root: Vertex<'Key, 'Value>) =
        public new() = AVLTree(Nil)

        interface IEnumerable<'Key * 'Value> with
            member _.GetEnumerator() = (treeSeq root).GetEnumerator()


        interface IEnumerable with
            member _.GetEnumerator() =
                (treeSeq root).GetEnumerator() :> IEnumerator

        member _.Height = height root

        member _.TryGet = tryGet root

        member _.Add (k: 'Key) v = AVLTree(insert k v root)

    let add k v (tree: AVLTree<'K, 'V>) = tree.Add k v

    let tryGet k (tree: AVLTree<'K, 'V>) = tree.TryGet k

    [<GeneralizableValue>]
    let empty<'K, 'V when 'K: comparison> : AVLTree<'K, 'V> = AVLTree<'K, 'V>()
