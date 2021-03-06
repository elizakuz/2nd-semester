﻿module Test

open NUnit.Framework
open System.IO
open progr
open parser
open mainTr

[<Test>]
let ``Only read and write`` () =
    let st = ";\nread\nx\nwrite\nx\n"
    let ar = [|2.0|]
    CLRfile (toTree(delExpr(parser(st))),size(parser(st)),ar)
    use stream = new StreamReader ("to.txt")
    Assert.AreEqual (stream.ReadToEnd(), "2\n")

[<Test>]
let ``V stepen`` () =
    let st = ";\nread\nx\n;\nread\nn\n;\n:=\nres\n1\n;\nwhile\nn\n;\n:=\nres\n*\nres\nx\n:=\nn\n-\nn\n1\nwrite\nres\n"
    let ar = [|2.0; 5.0|]
    CLRfile (toTree(delExpr(parser(st))),size(parser(st)),ar)
    use stream = new StreamReader ("to.txt")
    Assert.AreEqual (stream.ReadToEnd(), "32\n")
