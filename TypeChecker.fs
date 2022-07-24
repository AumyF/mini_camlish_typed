open System
open Parser
open FsToolkit.ErrorHandling

type Tyvar = string

type Ty =
  | TInt
  | TBool
  | TFn of Ty * Ty
  | TVar of Tyvar

type TyEnv = Map<string, Ty>
type TySubst = list<Tyvar * Ty>
let lookup tenv identifier = Map.tryFind identifier tenv

let ok_or e =
  function
  | Some (v) -> Ok(v)
  | None -> Error(e)

let typeVar s = TVar("'" + s)

let substitute tvar t tenv =
  option {
    let! key = Map.tryFindKey (fun _ value -> value = tvar) tenv
    return Map.add key t tenv
  }
  |> Option.defaultValue tenv

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

let rec occurs tx t =
  if tx = t then
    true
  else
    match t with
    | TFn (t1, t2) -> (occurs tx t1) || (occurs tx t2)
    | _ -> false

let rec subst_ty theta t =
  let rec subst_ty1 theta s =
    match theta with
    | [] -> TVar(s)
    | (tx, t1) :: theta -> if tx = s then t1 else subst_ty1 theta s

  match t with
  | TInt -> TInt
  | TBool -> TBool
  | TFn (tp, tb) -> TFn(subst_ty theta tp, subst_ty theta tb)
  | TVar (s) -> subst_ty1 theta s

let subst_tyenv theta tenv =
  List.map (fun (x, t) -> (x, subst_ty theta t)) tenv

let subst_eql theta eql =
  List.map (fun (t1, t2) -> (subst_ty theta t1, subst_ty theta t2)) eql

let rec lookup_list x ls =
  match ls with
  | [] -> None
  | (y, z) :: tl ->
    if x = y then
      Some(z)
    else
      lookup_list x tl

let rec compose_subst theta2 theta1 =
  let theta = List.map (fun (tx, t) -> (tx, subst_ty theta2 t)) theta1

  List.fold
    (fun tau ->
      fun (tx, t) ->
        match lookup_list tx theta1 with
        | Some (_) -> tau
        | None -> (tx, t) :: tau)
    theta
    theta2

let unify eql =
  let rec solve eql theta =
    match eql with
    | [] -> Ok(theta)
    | (t1, t2) :: eql ->
      if t1 = t2 then
        solve eql theta
      else
        match (t1, t2) with
        | (TFn (t1p, t1b), TFn (t2p, t2b)) -> solve ((t1p, t2p) :: (t1b, t2b) :: eql) theta
        | (TVar (s), _) ->
          if (occurs t1 t2) then
            Error("unification failed: t1 occurs in t2")
          else
            solve (subst_eql [ (s, t2) ] eql) (compose_subst [ (s, t2) ] theta)
        | (_, TVar (s)) ->
          if (occurs t2 t1) then
            Error("unification failed: t2 occurs in t1")
          else
            solve (subst_eql [ (s, t1) ] eql) (compose_subst [ (s, t2) ] theta)
        | (_, _) -> Error("unification failed")

  solve eql []


let test tenv expr =
  printfn "Typechecking %A:" expr
  let result = typecheck1 tenv expr
  printfn "  %A" result

let test_unify eql =
  printfn "Unifying %A:" eql
  let result = unify eql
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

  [ [ (TVar("'a"), TBool) ]
    [ (TInt, TBool) ]
    [ (TVar("'a"), TVar("'b")) ]
    [ (TFn(TVar("'a"), TVar("'b")), TFn(TVar("'b"), TVar("'c"))) ]
    [ (TVar("'a"), TFn(TVar("'b"), TVar("'a"))) ] ]
  |> List.iter (test_unify)

  0
