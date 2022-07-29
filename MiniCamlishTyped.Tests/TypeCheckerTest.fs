module MiniCamlishTyped.Tests

open System
open Parser
open TypeChecker
open FsToolkit.ErrorHandling
open NUnit.Framework
open FsUnitTyped

[<SetUp>]
let setup () =
  FsUnit.TopLevelOperators.FSharpCustomMessageFormatter()
  |> ignore

[<Test>]
let compose_1 () =
  compose_subst [ "'a", TInt ] [
    "'b", TBool
  ]
  |> shouldEqual [ "'a", TInt; "'b", TBool ]

[<Test>]
let substTy1 () =
  subst_ty [ "a", TInt ] (TVar "a")
  |> shouldEqual TInt

[<Test>]
let substTy2 () =
  subst_ty [ "a", TInt; "b", TBool ] (TFn(TVar "a", TVar "b"))
  |> shouldEqual (TFn(TInt, TBool))

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

[<Test>]
let Test2 () =
  result {
    let! (_, ty, _) =
      typecheck1
        (Map [])
        (Function(
          "f",
          Function("x", Apply(IdentifierReference "f", Apply(IdentifierReference "f", IdentifierReference "x")))
        ))

    return
      ty
      |> shouldEqual (TFn(TFn(TVar "'a3", TVar "'a3"), TFn(TVar "'a3", TVar "'a3")))
  }
  |> ignore


[<Test>]
let Test3 () =
  result {
    let! (_, ty, _) =
      typecheck1
        (Map [])
        (If(IdentifierReference "p", Add(IdentifierReference "x", IntegerLiteral(12)), IdentifierReference "y"))

    return ty |> shouldEqual TInt
  }
  |> ignore
