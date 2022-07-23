open System
open Parser
open FsToolkit.ErrorHandling

type Ty =
  | TInt
  | TBool
  | TFn of Ty * Ty

type TyEnv = Map<string, Ty>
let lookup tenv identifier = Map.tryFind identifier tenv

let ok_or e =
  function
  | Some (v) -> Ok(v)
  | None -> Error(e)

let rec typecheck1 tenv expr =
  match expr with
  | IdentifierReference (name) ->
    lookup tenv name
    |> ok_or (sprintf "no identifier %s found in type env" name)
  | IntegerLiteral (_) -> Ok(TInt)
  | BoolLiteral (_) -> Ok(TBool)
  | Add (e1, e2) ->
    let tc = typecheck1 tenv

    match (tc e1, tc e2) with
    | (Ok (TInt), Ok (TInt)) -> Ok(TInt)
    | _ -> Error("type error in add")
  | If (econd, etrue, efalse) ->
    let tc = typecheck1 tenv

    match (tc econd, tc etrue, tc efalse) with
    | (Ok (TBool), Ok (TInt), Ok (TInt)) -> Ok(TInt)
    | (Ok (TBool), Ok (TBool), Ok (TBool)) -> Ok(TBool)
    | (Ok (TBool), Ok (t1), Ok (t2)) -> Error(sprintf "type mismatches between result expressions: %A %A" t1 t2)
    | (Ok (t), _, _) -> Error(sprintf "expected bool, got %A" t)
    | (t1, t2, t3) -> Error(sprintf "type error: %A %A %A " t1 t2 t3)

  | Function (param, body) ->
    result {
      let! t1 =
        lookup tenv param
        |> Result.requireSome (sprintf "type env error")

      let! t2 = typecheck1 tenv body
      return TFn(t1, t2)
    }
  | Apply (fn, arg) ->
    let checkFn tfn targ =
      match tfn with
      | TFn (tparam, tbody) when tparam = targ -> Ok(tbody)
      | TFn (tparam, _) -> Error(sprintf "type of param and arg mismatches; param: %A arg: %A" tparam targ)
      | tfn -> Error(sprintf "expected function but got %A" tfn)

    result {
      let! tfn = typecheck1 tenv fn
      let! targ = typecheck1 tenv arg
      let! t = checkFn tfn targ
      return t
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

  let tenv = Map [ ("x", TInt); ("p", TBool) ]

  [ If(IdentifierReference("p"), IntegerLiteral(1), IdentifierReference("x"))
    If(IdentifierReference("x"), IntegerLiteral(3), IntegerLiteral(12))
    Function("x", If(BoolLiteral(true), IdentifierReference("x"), IntegerLiteral(100))) ]
  |> List.iter (test tenv)

  0
