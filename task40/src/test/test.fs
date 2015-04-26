module test
open Modul
open NUnit.Framework

[<TestCase (2, Result = 0)>]
[<TestCase (3, Result = 1)>]
[<TestCase (11, Result = 10)>]
let ``test 10`` i =
    modul i {
         let! a = 2 * 3
         let! b = 4
         return a + b
    }

[<TestCase (2, Result = 0)>]
[<TestCase (3, Result = 2)>]
[<TestCase (11, Result = 1)>]
let ``test -10`` i =
    modul i {
         let! a = -2 * 3
         let! b = -4
         return a + b
    }

