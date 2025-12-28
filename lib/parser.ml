open Types

let parseTokens (_:(token list)): expr = ExprTerm(
  MulOp(
    TermFactor(FactorUnary(UnaryPrimary(Groupe(
      AddOp(
        ExprTerm(TermFactor(FactorUnary(UnaryPrimary(Nombre 2.0)))),
        ADD,
        TermFactor(FactorUnary(UnaryPrimary(Nombre 3.0)))
      )
    )))),
    MUL,
    FactorUnary(UnaryPrimary(Nombre 4.0))
  )
) 