module test
open NUnit.Framework
open max

[<TestCase (1, [|1;1;1;1;1;1;1;1;123;1;1;1;1;1;1|], Result = 123)>]
[<TestCase (2, [|1;1;1;1;1;1;1;1;123;1;1;1;1;1;1|], Result = 123)>]
[<TestCase (3, [|1;1;1;1;1;1;1;1;123;1;1;1;1;1;1|], Result = 123)>]
[<TestCase (15, [|1;1;1;1;1;1;1;1;123;1;1;1;1;1;1|], Result = 123)>]
let test i arr = max i arr

[<TestCase (1, [|1|], Result = 1)>]
[<TestCase (2, [|1|], Result = 1)>]
[<TestCase (10, [|1|], Result = 1)>]
let ``Only one elem`` i arr = max i arr

[<TestCase (1, Result = 99999)>]
[<TestCase (2, Result = 99999)>]
[<TestCase (3, Result = 99999)>]
[<TestCase (100, Result = 99999)>]
[<TestCase (10000, Result = 99999)>]
let ``long array`` i = max i (Array.init 100000 (fun i -> i))
