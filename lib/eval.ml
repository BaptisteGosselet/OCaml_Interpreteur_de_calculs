open Types

let rec eval (exp:expr) : float = 
  let eval_primary (p : primary) : float = 
    match p with 
    | Nombre n -> n 
    | Groupe e -> (eval e)
  in

  let rec eval_unary (u : unary) : float = 
    match u with 
    | UnaryPrimary p -> (eval_primary p)
    | UnaryOp (NEG, u') -> -. (eval_unary u')
  in

  let eval_factor (f: factor) : float = 
    match f with 
    | FactorUnary u -> (eval_unary u)
  in

  let rec eval_term (t: term) : float = 
    match t with 
    | TermFactor f -> (eval_factor f)
    | MulOp (t', op, f) -> 
        let g = (eval_term t') in
        let d = (eval_factor f) in 
        match op with 
        | MUL -> g *. d
        | DIV -> g /. d
  in

  let rec eval_expr (e: expr): float = 
    match e with 
    | ExprTerm t -> (eval_term t)
    | AddOp (e', op, t) ->
      let g = (eval_expr e') in
      let d = (eval_term t) in
      match op with 
      | ADD -> g +. d
      | SUB -> g -. d
  in

  eval_expr exp
