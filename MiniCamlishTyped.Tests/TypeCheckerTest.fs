module MiniCamlishTyped.Tests

open System
open Parser
open TypeChecker
open NUnit.Framework
open FsUnitTyped

[<SetUp>]
let setup () =
  FsUnit.TopLevelOperators.FSharpCustomMessageFormatter()
  |> ignore

[<Test>]
let Test1 () =
  unify [ (TVar "a", TBool) ]
  |> shouldEqual (Ok[("a", TBool)])

[<Test>]
let ``Fails to unify incompatible types`` () =
  unify [ (TInt, TBool) ]
  |> shouldEqual (Error "unification failed")

[<Test>]
let ``Succeeds to unify type variables`` () =
  unify [ TVar("a"), TVar("b") ]
  |> shouldEqual (Ok [ "a", TVar("b") ])

[<Test>]
let ``Succeeds to unify function types`` () =
  unify [ TFn(TVar "a", TVar "b"), TFn(TVar "b", TVar "c") ]
  |> shouldEqual (Ok [ "b", TVar "c"; "a", TVar "c" ])

[<Test>]
let ``Fails to unify because of `occurs` `` () =
  unify [ TVar "a", TFn(TVar "b", TVar "a") ]
  |> shouldEqual (Error "unification failed: t1 occurs in t2")
