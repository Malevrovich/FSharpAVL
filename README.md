`Скрябин Иван P34092`
`335146`

# Требования
Функции:
- добавление и удаление элементов;
- фильтрация;
- отображение (map);
- свертки (левая и правая);
- структура должна быть моноидом.

Требования:
- Структуры данных должны быть неизменяемыми.
- Библиотека должна быть протестирована в рамках unit testing.
- Библиотека должна быть протестирована в рамках property-based тестирования (как минимум 3 свойства, включая свойства моноида).
- Структура должна быть полиморфной.
- Требуется использовать идиоматичный для технологии стиль программирования. Примечание: некоторые языки позволяют получить большую часть API через реализацию небольшого интерфейса. Так как лабораторная работа про ФП, а не про экосистему языка -- необходимо реализовать их вручную и по возможности -- обеспечить совместимость.

Вариант: `avl-dict`

# Ключевые элементы реализации

Внутренний тип:
```f#
type private Vertex<'K, 'V> =
    | Node of int * int * 'K * 'V * Vertex<'K, 'V> * Vertex<'K, 'V> // count, height, key, value, l-child, r-child
    | Nil
```

Вспомогательная функция-конструктор:
```f#
let private createVertex k v l r =
    Node(1 + count l + count r, 1 + max (height l) (height r), k, v, l, r)
```

Балансировка:
```f#
let private diff node =
    match node with
    | Node(_, _, _, _, l, r) -> height l - height r
    | Nil -> 0

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
```

Функция вставки:
```f#
let rec private insert k v node =
    match node with
    | Node(_, _, k', v', l', r') when k' = k -> createVertex k v l' r'
    | Node(_, _, k', v', l', r') ->
        let l, r = if k < k' then insert k v l', r' else l', insert k v r'
        createVertex k' v' l r |> balance
    | Nil -> createVertex k v Nil Nil
```

Функция удаления:
```f#
let rec private remove k node =
    let rec extractLeast node =
        match node with
        | Node(_, _, k, v, Nil, r) -> r, k, v
        | Node(_, _, k, v, l, r) ->
            let remainder, leastK, leastV = extractLeast l
            createVertex k v remainder r, leastK, leastV
        | Nil -> raise (System.ArgumentNullException())

    let deleteRoot node =
        match node with
        | Node(_, _, _, _, Nil, Nil) -> Nil
        | Node(_, _, _, _, Nil, r) -> r
        | Node(_, _, _, _, l, Nil) -> l
        | Node(_, _, _, _, l, r) ->
            let remainder, leastK, leastV = extractLeast r
            createVertex leastK leastV l remainder
        | Nil -> raise (System.ArgumentNullException())

    match node with
    | Node(_, _, k', v', l', r') when k' = k -> deleteRoot node |> balance
    | Node(_, _, k', v', l', r') ->
        let l, r = if k < k' then remove k l', r' else l', remove k r'
        createVertex k' v' l r |> balance
    | Nil -> Nil
```

Функция добычи элемента:
```f#
let rec private tryGet node k =
    match node with
    | Node(_, _, k', v', _, _) when k = k' -> Some v'
    | Node(_, _, k', _, l', _) when k < k' -> tryGet l' k
    | Node(_, _, k', _, _, r') when k > k' -> tryGet r' k
    | _ -> None
```

Вывод дерева на языке Dot:
```f#
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
```

Отображение:
```f#
let rec private map node f =
    match node with
    | Node(_, _, k, v, l, r) -> createVertex k (f v) (map l f) (map r f)
    | Nil -> Nil
```

Итератор:
```f#
let rec private treeSeq tree =
    seq {
        match tree with
        | Node(_, _, k, v, l, r) ->
            yield! treeSeq l
            yield k, v
            yield! treeSeq r
        | Nil -> ()
    }
```

Публичный класс колекции:
```f#
type AVLTree<'Key, 'Value when 'Key: comparison and 'Value: equality> private (root: Vertex<'Key, 'Value>) =
    public new() = AVLTree(Nil)

    member _.Height = height root

    member _.TryGet = tryGet root

    member _.Add (k: 'Key) v = AVLTree(insert k v root)
    member _.Remove(k: 'Key) = AVLTree(remove k root)

    member _.Dump = dumpTree root

    member _.Map f = AVLTree(map root f)

    member _.BackSeq = treeSeqBack root

    member _.MaxDiff = maxDiff root

    member this.Item
        with get (k: 'Key): 'Value = tryGet root k |> Option.get

    interface IEnumerable<'Key * 'Value> with
        member _.GetEnumerator() = (treeSeq root).GetEnumerator()

    override this.Equals(other: obj) =
        if other :? AVLTree<'Key, 'Value> then
            Seq.zip this (other :?> AVLTree<'Key, 'Value>)
            |> Seq.map (fun (kv1, kv2) -> kv1 = kv2)
            |> Seq.fold (&&) true
        else
            false

    override this.GetHashCode() : int =
        this
        |> Seq.fold (fun res (k, v) -> HashCode.Combine(res, k.GetHashCode(), v.GetHashCode)) 0
```

Далее много где дерево превращается в Seq неявно через благодаря интерфейсу IEnumerable, в частности происходит создание итератора, реализация которого была приведена выше

Моноид на уровне API:
```f#
[<GeneralizableValue>]
let empty<'K, 'V when 'K: comparison and 'V: equality> : AVLTree<'K, 'V> =
    AVLTree<'K, 'V>()

let merge (lhs: AVLTree<'K, 'V>) (rhs: AVLTree<'K, 'V>) =
    lhs |> Seq.fold (fun tree (k, v) -> tree |> add k v) rhs
```

Конструктор по перечислению:
```f#
let ofItems (items: IEnumerable<'K * 'V>) =
    items |> Seq.fold (fun tree (k, v) -> tree |> add k v) empty
```

Фильтрация и свертки:
```f#
let filter pred (tree: AVLTree<'K, 'V>) = tree |> Seq.filter pred |> ofItems

let fold f state (tree: AVLTree<'K, 'V>) = tree |> Seq.fold f state
let foldBack f state (tree: AVLTree<'K, 'V>) = tree.BackSeq |> Seq.fold f state
```

# Тесты:
Была использована библиотека FsCheck позволяющая осуществить property-based тестирование

Сами тесты перечислены в логическом порядке в файле:
https://github.com/Malevrovich/FSharpAVL/blob/master/tests/FSharpAVLTests/Tests.fs

Вывод тестов:
```
Run dotnet test --no-build --logger "console;verbosity=normal"
Test run for /home/runner/work/FSharpAVL/FSharpAVL/tests/FSharpAVLTests/bin/Debug/net6.0/FSharpAVLTests.dll (.NETCoreApp,Version=v6.0)
Microsoft (R) Test Execution Command Line Tool Version 17.3.3 (x64)
Copyright (c) Microsoft Corporation.  All rights reserved.

Starting test execution, please wait...
A total of 1 test files matched the specified pattern.
[xUnit.net 00:00:00.00] xUnit.net VSTest Adapter v2.4.3+1b45f5407b (64-bit .NET 6.0.35)
[xUnit.net 00:00:01.37]   Discovering: FSharpAVLTests
[xUnit.net 00:00:01.41]   Discovered:  FSharpAVLTests
[xUnit.net 00:00:01.41]   Starting:    FSharpAVLTests
  Passed Tests.Tree filter equals to removes [104 ms]
  Passed Tests.Diff is -1, 0, 1 after remove [9 ms]
  Passed Tests.Element not found after remove [144 ms]
  Passed Tests.Merge test [67 ms]
  Passed Tests.Enumerate test [2 ms]
  Passed Tests.Diff is -1, 0, 1 after creation [4 ms]
  Passed Tests.Tree fold goes ascending [6 ms]
  Passed Tests.Equals test [177 ms]
  Passed Tests.Tree fold [8 ms]
  Passed Tests.Storage test [54 ms]
  Passed Tests.Merge property [28 ms]
  Passed Tests.Access test [< 1 ms]
  Passed Tests.Tree map [5 ms]
  Passed Tests.Merge empty test [< 1 ms]
  Passed Tests.Element not found [105 ms]
  Passed Tests.Monoid associative [8 ms]
  Passed Tests.Monoid neutral [4 ms]
  Passed Tests.My test [< 1 ms]
[xUnit.net 00:00:02.32]   Finished:    FSharpAVLTests
  Passed Tests.Tree filter [5 ms]
  Passed Tests.Storage property [25 ms]
  Passed Tests.Remove property [5 ms]
  Passed Tests.Tree foldBack goes descending [5 ms]

Test Run Successful.
Total tests: 22
     Passed: 22
 Total time: 2.7926 Seconds
```
