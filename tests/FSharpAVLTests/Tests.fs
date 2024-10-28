module Tests

open System

open Xunit
open FsCheck.Xunit
open AVL
open System.Collections.Generic

[<Fact>]
let ``My test`` () = Assert.True(true)

[<Fact>]
let ``Access test`` () =
    Assert.True(
        AVLTree.empty
        |> AVLTree.add 1 2
        |> AVLTree.add 2 4
        |> AVLTree.add 10 20
        |> AVLTree.add 5 10
        |> AVLTree.add 3 6
        |> AVLTree.tryGet 5 =
            Some 10
    )

[<Fact>]
let ``Enumerate test`` () =
    Assert.True(
        AVLTree.ofItems (List.zip [ 0..4 ] [ 0..4 ])
        |> Seq.map (fun (k, v) -> k)
        |> List.ofSeq =
            [ 0..4 ]
    )

let shuffle seq =
    let array = seq |> Seq.toArray
    let random = Random()

    for i in 0 .. array.Length - 1 do
        let j = random.Next(i, array.Length)
        let pom = array.[i]
        array.[i] <- array.[j]
        array.[j] <- pom

    array |> Array.toSeq

[<Fact>]
let ``Storage test`` () =
    let init = Seq.initInfinite (fun i -> i, i * i) |> Seq.take 10000
    let tree = init |> shuffle |> AVLTree.ofItems

    Assert.True([ -1; 0; 1 ] |> List.contains tree.MaxDiff)

    init |> Seq.map (fun (k, v) -> Assert.Equal(tree |> AVLTree.tryGet k, Some v))

[<Fact>]
let ``Equals test`` () =
    let init = Seq.initInfinite (fun i -> i, i * i) |> Seq.take 10000
    let l = init |> AVLTree.ofItems
    let r = init |> AVLTree.ofItems

    Assert.True((l = r))

    let x = init |> Seq.skip 1 |> AVLTree.ofItems
    Assert.False((l = x))
    Assert.False((x = r))

[<Fact>]
let ``Merge test`` () =
    let init = Seq.initInfinite (fun i -> i, i * i) |> Seq.take 10000

    let oddTree =
        init |> Seq.filter (fun (k, v) -> k % 2 = 1) |> shuffle |> AVLTree.ofItems

    let evenTree =
        init |> Seq.filter (fun (k, v) -> k % 2 = 0) |> shuffle |> AVLTree.ofItems

    let tree = AVLTree.merge oddTree evenTree

    init |> Seq.map (fun (k, v) -> Assert.Equal(tree |> AVLTree.tryGet k, Some v))

[<Fact>]
let ``Merge empty test`` () =
    let tree = AVLTree.ofItems (List.zip [ 0..5 ] [ 0..5 ])

    Assert.True((tree = (AVLTree.merge tree AVLTree.empty)))
    Assert.True((tree = (AVLTree.merge AVLTree.empty tree)))

[<Fact>]
let ``Element not found`` () =
    let init = Seq.initInfinite (fun i -> i, i * i) |> Seq.take 10000
    let tree = init |> shuffle |> AVLTree.ofItems

    init |> Seq.iter (fun (k, v) -> Assert.Equal(tree |> AVLTree.tryGet k, Some v))

    init
    |> Seq.iter (fun (k, v) -> Assert.Equal(tree |> AVLTree.tryGet (k + 10000), None))

[<Fact>]
let ``Element not found after remove`` () =
    let init = Seq.initInfinite (fun i -> i, i * i) |> Seq.take 10000
    let tree = init |> shuffle |> AVLTree.ofItems


    init |> Seq.iter (fun (k, v) -> Assert.Equal(tree |> AVLTree.tryGet k, Some v))

    let remainder = tree |> AVLTree.remove 500

    init
    |> Seq.iter (fun (k, v) -> Assert.Equal(remainder |> AVLTree.tryGet k, if k = 500 then None else Some v))


[<Property>]
let ``Storage property`` (l: string list) =
    let tree = l |> List.map (fun x -> x.GetHashCode()) |> List.zip l |> AVLTree.ofItems

    l
    |> Seq.iter (fun k -> Assert.Equal(tree |> AVLTree.tryGet k, Some(k.GetHashCode())))

[<Property>]
let ``Remove property`` (l: int list) =
    let tree = List.zip l l |> AVLTree.ofItems

    let evenTree =
        tree
        |> AVLTree.fold (fun tree (k, v) -> if k % 2 = 0 then tree else tree |> AVLTree.remove k) tree

    l
    |> Seq.iter (fun k -> Assert.Equal(evenTree |> AVLTree.tryGet k, if k % 2 = 0 then Some k else None))

[<Property>]
let ``Diff is -1, 0, 1 after creation`` (l: int list) =
    let tree = List.zip l l |> AVLTree.ofItems

    [ -1; 0; 1 ] |> List.contains tree.MaxDiff

[<Property>]
let ``Diff is -1, 0, 1 after remove`` (l: int list) =
    let tree = List.zip l l |> AVLTree.ofItems

    [ -1; 0; 1 ]
    |> List.contains
        (tree
         |> AVLTree.fold (fun tree (k, v) -> if k % 2 = 0 then tree else tree |> AVLTree.remove k) tree)
            .MaxDiff

[<Property>]
let ``Merge property`` (l: int list, r: int list) =
    let lTree = List.zip l l |> AVLTree.ofItems
    let rTree = List.zip r r |> AVLTree.ofItems

    let lrTree = l |> List.append r |> List.map (fun x -> x, x) |> AVLTree.ofItems

    Assert.True(((AVLTree.merge lTree rTree) = lrTree))

[<Property>]
let ``Monoid associative`` (a: int list) (b: int list) (c: int list) =
    let aTree = List.zip a a |> AVLTree.ofItems
    let bTree = List.zip b b |> AVLTree.ofItems
    let cTree = List.zip c c |> AVLTree.ofItems

    let lhs = AVLTree.merge aTree (AVLTree.merge bTree cTree)
    let rhs = AVLTree.merge (AVLTree.merge aTree bTree) cTree

    Assert.True((lhs = rhs))

[<Property>]
let ``Monoid neutral`` (l: int list) =
    let tree = List.zip l l |> AVLTree.ofItems

    Assert.True((tree = (AVLTree.merge tree AVLTree.empty)))
    Assert.True((tree = (AVLTree.merge AVLTree.empty tree)))

[<Property>]
let ``Tree fold`` (l: int list) =
    let tree = l |> List.map (fun x -> x * x) |> List.zip l |> AVLTree.ofItems

    let expected = l |> Set.ofList |> Set.fold (fun x y -> x + y * y) 0
    let actual = tree |> AVLTree.fold (fun x (k, v) -> x + k * k) 0

    Assert.True((actual = expected))

[<Property>]
let ``Tree fold goes ascending`` (l: int list) =
    let tree = List.zip l l |> AVLTree.ofItems

    Assert.True(
        tree
        |> AVLTree.fold (fun l (k, v) -> List.append l [ (k, v) ]) []
        |> Seq.pairwise
        |> Seq.forall (fun ((k1, v1), (k2, v2)) -> k1 <= k2)
    )

[<Property>]
let ``Tree foldBack goes descending`` (l: int list) =
    let tree = List.zip l l |> AVLTree.ofItems

    Assert.True(
        tree
        |> AVLTree.foldBack (fun l (k, v) -> List.append l [ (k, v) ]) []
        |> Seq.pairwise
        |> Seq.forall (fun ((k1, v1), (k2, v2)) -> k1 >= k2)
    )

[<Property>]
let ``Tree map`` (l: int list) =
    let tree = List.zip l l |> AVLTree.ofItems

    let mappedTree = tree |> AVLTree.map (fun x -> x * x)
    let expectedTree = l |> List.map (fun x -> x * x) |> List.zip l |> AVLTree.ofItems

    Assert.True((mappedTree = expectedTree))

[<Property>]
let ``Tree filter`` (l: int list) =
    let tree = List.zip l l |> AVLTree.ofItems

    let filteredTree = tree |> AVLTree.filter (fun (k, v) -> k % 2 = 0)

    let expectedTree =
        List.zip l l |> List.filter (fun (k, v) -> k % 2 = 0) |> AVLTree.ofItems

    Assert.True((filteredTree = expectedTree))

[<Property>]
let ``Tree filter equals to removes`` (l: int list) =
    let tree = List.zip l l |> AVLTree.ofItems

    let filteredTree = tree |> AVLTree.filter (fun (k, v) -> k % 2 = 0)

    let treeRemainder =
        tree
        |> AVLTree.fold (fun tree (k, v) -> if k % 2 = 0 then tree else tree |> AVLTree.remove k) tree

    Assert.True((filteredTree = treeRemainder))
