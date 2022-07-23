open System
open Parser

type Ty =
  | TInt
  | TBool

let rec typecheck1 expr =
  match expr with
  | IntegerLiteral (_) -> Ok(TInt)
  | BoolLiteral (_) -> Ok(TBool)
  | Add (e1, e2) ->
    match (typecheck1 e1, typecheck1 e2) with
    | (Ok (TInt), Ok (TInt)) -> Ok(TInt)
    | _ -> Error("type error in add")
  | If (econd, etrue, efalse) ->
    match (typecheck1 econd, typecheck1 etrue, typecheck1 efalse) with
    | (Ok (TBool), Ok (TInt), Ok (TInt)) -> Ok(TInt)
    | (Ok (TBool), Ok (TBool), Ok (TBool)) -> Ok(TBool)
    | (Ok(TBool), Ok (t1), Ok (t2)) -> Error(sprintf "type mismatches between result expressions: %A %A" t1 t2)
    | (Ok (t), _, _) -> Error(sprintf "expected bool, got %A" t)
    | (t1, t2, t3) -> Error(sprintf "type error: %A %A %A " t1 t2 t3)


let test expr =
  printfn "Typechecking %A:" expr
  let result = typecheck1 expr
  printfn "  %A" result

[<EntryPoint>]
let main argv =
  [ If(BoolLiteral(true), IntegerLiteral(1), IntegerLiteral(100))
    If(BoolLiteral(true), IntegerLiteral(1), BoolLiteral(true)) ]
  |> List.iter (test)

  0
