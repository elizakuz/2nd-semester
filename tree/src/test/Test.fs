module test

open tree
open NUnit.Framework

[<Test>]
let ``combineTest1`` () =
    let tr = T (Nil, 1, Nil)
    let t = T (Nil, 2, Nil)
    Assert.AreEqual (tree.Combine (tr, t), T (Nil, 1, T (Nil, 2, Nil)))

[<Test>]
let ``combineTest free brunch`` () =
    let tr = T (T (Nil, 1, Nil), 3, Nil)
    let t = T (Nil, 2, Nil)
    Assert.AreEqual (tree.Combine (tr, t), T (T (T (Nil, 1, Nil), 3, Nil), 2, Nil))

[<Test>]
let ``mapTest1 Nil`` () =
    let tr = Nil
    let result = Nil
    Assert.AreEqual (map ((+) 1) tr, result)

[<Test>]
let ``mapTest2 * 2`` () =
    let tr = T (T (Nil, 1, Nil), 5, T (Nil, 8, Nil))
    let result = T (T (Nil, 2, Nil), 10, T (Nil, 16, Nil))
    Assert.AreEqual (map (fun x -> x * 2) tr, result)

[<Test>]
let ``filterTest < 6`` () =
    let tr = T (T (Nil, 1, T (Nil, 3, Nil)), 5, T (Nil, 8, T (Nil, 9, Nil)))
    let result = T (T (Nil, 1, T (Nil, 3, Nil)), 5, Nil)
    Assert.AreEqual (filter (fun x -> x < 6) tr, result)



