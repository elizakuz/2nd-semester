module test
open NUnit.Framework
open integr

[<TestCase (0.0, 1.0, 1.0, 1, Result = 0.5)>]
[<TestCase (1.0, 6.0, 2.0, 2, Result = 17.5)>]
[<TestCase (0.0, 1000.0, 3.5, 3, Result = 500000.0)>]
let ``x`` a b otrezok flowsNum = integr ("x", a, b, otrezok, flowsNum)

[<TestCase (0.5, 1.0, 1.0, 1, Result = 3.8125)>]
[<TestCase (0.5, 1.0, 2.0, 2, Result = 3.8125)>]
[<TestCase (0.5, 1.0, 3.75, 100, Result = 3.8125)>]
let ``(x + 2) ^ 2`` a b otrezok flowsNum = integr ("(x + 2)^2", a, b, otrezok, flowsNum)