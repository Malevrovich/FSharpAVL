module Tests

open System

open Xunit
open AVL
open System.Collections.Generic

[<Fact>]
let ``My test`` () = Assert.True(true)

[<Fact>]
let ``AccessTest`` () =
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
let ``EnumerateTest`` () =
    Assert.True(
        Seq.ofList [ 0..4 ]
        |> Seq.fold (fun tree el -> tree |> AVLTree.add el el) AVLTree.empty
        |> Seq.map (fun (k, v) -> k)
        |> List.ofSeq =
            [ 0..4 ]
    )
