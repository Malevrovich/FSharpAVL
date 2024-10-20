module Tests

open System

open Xunit
open AVL

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
