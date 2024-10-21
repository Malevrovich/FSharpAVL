module Tests

open System

open Xunit
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

    Assert.True(
        init
        |> Seq.map (fun (k, v) -> tree |> AVLTree.tryGet k = Some v)
        |> Seq.reduce (&&)
    )

[<Fact>]
let ``Merge test`` () =
    let init = Seq.initInfinite (fun i -> i, i * i) |> Seq.take 10000

    let oddTree =
        init |> Seq.filter (fun (k, v) -> k % 2 = 1) |> shuffle |> AVLTree.ofItems

    let evenTree =
        init |> Seq.filter (fun (k, v) -> k % 2 = 0) |> shuffle |> AVLTree.ofItems

    let tree = AVLTree.merge oddTree evenTree

    Assert.True(
        init
        |> Seq.map (fun (k, v) -> tree |> AVLTree.tryGet k = Some v)
        |> Seq.reduce (&&)
    )

[<Fact>]
let ``Merge empty test`` () =
    let tree = AVLTree.ofItems (List.zip [ 0..5 ] [ 0..5 ])

    Assert.True(Seq.compareWith (fun x y -> if x = y then 0 else -1) tree (AVLTree.merge tree AVLTree.empty) = 0)
    Assert.True(Seq.compareWith (fun x y -> if x = y then 0 else -1) tree (AVLTree.merge AVLTree.empty tree) = 0)

[<Fact>]
let ``Element not found`` () =
    let init = Seq.initInfinite (fun i -> i, i * i) |> Seq.take 10000
    let tree = init |> shuffle |> AVLTree.ofItems

    Assert.True(
        init
        |> Seq.map (fun (k, v) -> tree |> AVLTree.tryGet k = Some v)
        |> Seq.reduce (&&)
    )

    Assert.True(
        init
        |> Seq.map (fun (k, v) -> tree |> AVLTree.tryGet (k + 10000) = None)
        |> Seq.reduce (&&)
    )
