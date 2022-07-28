module Main

open Parser
open TypeChecker

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
