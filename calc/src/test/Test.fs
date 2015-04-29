module test
open calc

open NUnit.Framework

[<TestCase ("1 + 2", Result = 3.0)>]
[<TestCase ("1 - 2", Result = -1.0)>]
[<TestCase ("2 ^ (1 + 2)", Result = 8.0)>]
[<TestCase ("1 / 2", Result = 0.5)>]
[<TestCase ("1 % 2", Result = 1.0)>]
let test1 st =
    Obhod (makeTree (MapIt (parser (st), [])))