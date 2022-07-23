module Parser

open FParsec

type Expression =
  | BoolLiteral of bool
  | IntegerLiteral of int
  | Add of Expression * Expression
  | If of Expression * Expression * Expression
  | IdentifierReference of string

let expression, expressionRef = createParserForwardedToRef<Expression, unit> ()

let pTrue = stringReturn "true" (BoolLiteral true)

let pFalse = stringReturn "false" (BoolLiteral false)

let boolLiteral = (pTrue <|> pFalse) .>> spaces
let integerLiteral = pint32 .>> spaces |>> IntegerLiteral

let identifierReference =
  identifier (IdentifierOptions()) .>> spaces
  |>> IdentifierReference

let expr1 =
  integerLiteral
  <|> boolLiteral
  <|> identifierReference

let plus = pstring "+" .>> spaces

let add = chainl1 expr1 (plus >>% (fun x y -> Add(x, y)))

do expressionRef.Value <- add <|> expr1

type Value =
  | Integer of int
  | Float of float
  | String of string
