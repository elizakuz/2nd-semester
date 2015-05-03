module test

open max
open NUnit.Framework

[<TestCase (1, [|1;1;1;1;1;1;1;1;123;1;1;1;1;1;1|], Result = 123)>]
[<TestCase (2, [|1;1;1;1;1;1;1;1;123;1;1;1;1;1;1|], Result = 123)>]
[<TestCase (3, [|1;1;1;1;1;1;1;1;123;1;1;1;1;1;1|], Result = 123)>]
[<TestCase (15, [|1;1;1;1;1;1;1;1;123;1;1;1;1;1;1|], Result = 123)>]
let test i arr = max i arr

[<TestCase (1, [|1|], Result = 1)>]
[<TestCase (2, [|1|], Result = 1)>]
[<TestCase (10, [|1|], Result = 1)>]
let ``Only one elem`` i arr = max i arr

