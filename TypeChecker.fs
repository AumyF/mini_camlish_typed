open System
open Parser
open FsToolkit.ErrorHandling

type Ty =
  | TInt
  | TBool
  | TVar of string

type TyEnv = Map<string, Ty>
let lookup tenv identifier = Map.tryFind identifier tenv

let ok_or e =
  function
  | Some (v) -> Ok(v)
  | None -> Error(e)

let typeVar s = TVar("'" + s)

let substitute tvar t tenv =
  option {
    let! key = Map.tryFindKey(fun _ value -> value = tvar) tenv
    return Map.add key t tenv
  } |> Option.defaultValue tenv

let rec typecheck1 tenv expr =
  match expr with
  | IdentifierReference (name) ->
    match lookup tenv name with
    | Some (ty) -> Ok(tenv, ty)
    | None ->
      let tvar = typeVar name
      let te1 = tenv.Add(name, tvar)
      Ok(te1, tvar)
  | IntegerLiteral (_) -> Ok(tenv, TInt)
  | BoolLiteral (_) -> Ok(tenv, TBool)
  | Add (e1, e2) ->

    let inferAndExtend t tenv =
      match t with
      | TInt -> Ok(tenv)
      | TVar (s) -> Ok(substitute t TInt tenv)
      | _ -> Error("type error in add")

    result {
      let! (te1, t1) = typecheck1 tenv e1
      let! te2 = inferAndExtend t1 te1
      let! (te3, t2) = typecheck1 te2 e2
      let! te4 = inferAndExtend t2 te3
      return (te4, TInt)
    }

  | If (econd, etrue, efalse) ->
    let inferAndExtendPredicate t tenv =
      match t with
      | TBool -> Ok(tenv)
      | TVar (s) -> Ok(substitute t TBool tenv)
      | _ -> Error("Expected bool")

    let inferAndExtendBody tt tf tenv =
      match (tt, tf) with
      | (TInt, TInt) -> Ok(tenv, TInt)
      | (TBool, TBool) -> Ok(tenv, TBool)
      | (TInt, TVar (s)) -> Ok(substitute tf TInt tenv, TInt)
      | (TVar (s), TInt) -> Ok(substitute tt TInt tenv, TInt)
      | (TBool, TVar (s)) -> Ok(substitute tf TBool tenv, TBool)
      | (TVar (s), TBool) -> Ok(substitute tt TBool tenv, TBool)
      | (TVar (s), TVar (t)) ->
        let tenv = substitute tt tf tenv
        Ok(tenv, tf)
      | _ -> Error("type error in if")

    result {
      let! (tenv, tcond) = typecheck1 tenv econd
      let! tenv = inferAndExtendPredicate tcond tenv
      let! (tenv, ttrue) = typecheck1 tenv etrue
      let! (tenv, tfalse) = typecheck1 tenv efalse
      let! (tenv, tresult) = inferAndExtendBody ttrue tfalse tenv
      return (tenv, tresult)
    }


let test tenv expr =
  printfn "Typechecking %A:" expr
  let result = typecheck1 tenv expr
  printfn "  %A" result

[<EntryPoint>]
let main argv =
  let tenv = Map []

  [ If(BoolLiteral(true), IntegerLiteral(1), IntegerLiteral(100))
    If(BoolLiteral(true), IntegerLiteral(1), BoolLiteral(true)) ]
  |> List.iter (test tenv)

  let tenv = Map []

  [ If(IdentifierReference("p"), IntegerLiteral(1), IdentifierReference("x"))
    If(IdentifierReference("x"), IntegerLiteral(3), IntegerLiteral(12)) ]
  |> List.iter (test tenv)

  0
