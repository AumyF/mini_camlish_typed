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

let substitute tvar t tenv =
  option {
    let! key = Map.tryFindKey (fun _ value -> value = tvar) tenv
    return Map.add key t tenv
  }
  |> Option.defaultValue tenv

let theta0 = ([]: TySubst)

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
  Map.map (fun x t -> (subst_ty theta t)) tenv

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

let rec typecheck1 tenv expr =
  let mutable typeVarCount = 0
  let newTypeVar () =
    let name= "'a" + typeVarCount.ToString()
    typeVarCount <- typeVarCount + 1
    TVar(name)
  match expr with
  | IdentifierReference (name) ->
    match lookup tenv name with
    | Some (ty) -> Ok(tenv, ty, theta0)
    | None ->
      let tvar = newTypeVar ()
      let tenv = Map.add name tvar tenv
      Ok(tenv, tvar, theta0)
  | IntegerLiteral (_) -> Ok(tenv, TInt, theta0)
  | BoolLiteral (_) -> Ok(tenv, TBool, theta0)
  | Add (eleft, eright) ->
    result {
      let! (tenv, tleft, theta1) = typecheck1 tenv eleft
      let! (tenv, tright, theta2) = typecheck1 tenv eright
      let tleft = subst_ty theta2 tleft
      let! theta3 = unify [ (tleft, TInt); (tright, TInt) ]
      let tenv = subst_tyenv theta3 tenv
      let theta = compose_subst theta3 (compose_subst theta2 theta1)
      return (tenv, TInt, theta)
    }
  | Function (param, ebody) ->
    result {
      let tparam = newTypeVar ()
      let tenv = Map.add param tparam tenv
      let! (tenv, tbody, theta1) = typecheck1 tenv ebody
      let tparam = subst_ty theta1 tparam
      let tenv = Map.remove param tenv
      return (tenv, TFn(tparam, tbody), theta1)
    }
  | Apply (efn, earg) ->
    result {
      let! (tenv, tfn, theta1) = typecheck1 tenv efn
      let! (tenv, targ, theta2) = typecheck1 tenv earg
      let tvar = newTypeVar ()
      let tfn = subst_ty theta2 tfn
      let! theta3 = unify [ (tfn, TFn(targ, tvar)) ]
      let tret = subst_ty theta3 tvar
      let tenv = subst_tyenv theta3 tenv
      let theta4 = compose_subst theta3 (compose_subst theta2 theta1)
      return (tenv, tret, theta4)
    }
  | _ -> Error("unknown expression")


//| If (econd, etrue, efalse) ->
//result {
//  let! (tenv, tcond, theta1) = typecheck1 tenv econd
// let! (tenv, ttrue, theta2) = typecheck1 tenv etrue
//let! (tenv, tfalse, theta3) = typecheck1 tenv efalse
// let ttrue = subst_ty theta3 ttrue
// let! theta4= unify
// }

let rec showType ty =
  match ty with
  | TInt -> "int"
  | TBool -> "bool"
  | TFn(p, b) ->
    let p = showType p
    let b = showType b
    sprintf "%s -> %s" p b
  | TVar(name) -> name

let checktop e = typecheck1 (Map []) e

let test tenv expr =
  printfn "Typechecking %A:" expr
  let result = typecheck1 tenv expr
  match result with
  | Ok(tenv, texpr, theta) -> printfn "  %A" (showType texpr)
  | Error(_) -> printfn "  %A" result

let test_unify eql =
  printfn "Unifying %A:" eql
  let result = unify eql
  printfn "  %A" result
    

[<EntryPoint>]
let main argv =
  [
    // If(BoolLiteral true, IntegerLiteral 1, IntegerLiteral 100)
    // If(IdentifierReference "x", Add(IdentifierReference "y", IntegerLiteral 10), IntegerLiteral 100) ;
    Function("a", Function("b", Add(IdentifierReference "a", IdentifierReference "b")))
    Function(
      "f",
      Function("x", Apply(IdentifierReference "f", Apply(IdentifierReference "f", IdentifierReference "x")))
    )
    Function(
      "x",
      Function(
        "y",
        Function(
          "z",
          Apply(
            Apply(IdentifierReference "x", IdentifierReference "z"),
            Apply(IdentifierReference "y", IdentifierReference "z")
          )
        )
      )
    ) ]
  |> List.iter (test (Map []))

  0
