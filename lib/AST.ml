exception TODO

module F = Format

module type NodeSig = sig
  type t

  val token_literal : t -> string

  val to_string : t -> string
end

module MakeExpression (SubType : NodeSig) = struct
  type t = SubType.t

  let token_literal : t -> string = SubType.token_literal

  let to_string : t -> string = SubType.to_string
end

module MakeStatement (SubType : NodeSig) = struct
  type t = SubType.t

  let token_literal : t -> string = SubType.token_literal

  let to_string : t -> string = SubType.to_string
end

module rec Expression : NodeSig = struct
  type t =
    | Identifier of Identifier.t
    | Boolean of Boolean.t
    | Integer of Integer.t
    | Prefix of Prefix.t
    | Infix of Infix.t
    | If of If.t
    | Fn of Fn.t
    | Call of Call.t

  let token_literal (expression : t) : string =
    match expression with
    | Identifier identifier_expr ->
        Identifier.token_literal identifier_expr
    | Boolean boolean_expr ->
        Boolean.token_literal boolean_expr
    | Integer integer_expr ->
        Integer.token_literal integer_expr
    | Prefix prefix_expr ->
        Prefix.token_literal prefix_expr
    | Infix infix_expr ->
        Infix.token_literal infix_expr
    | If if_expr ->
        If.token_literal if_expr
    | Fn fn_expr ->
        Fn.token_literal fn_expr
    | Call call_expr ->
        Call.token_literal call_expr


  let to_string (expression : t) : string =
    match expression with
    | Identifier identifier_expr ->
        Identifier.to_string identifier_expr
    | Boolean boolean_expr ->
        Boolean.to_string boolean_expr
    | Integer integer_expr ->
        Integer.to_string integer_expr
    | Prefix prefix_expr ->
        Prefix.to_string prefix_expr
    | Infix infix_expr ->
        Infix.to_string infix_expr
    | If if_expr ->
        If.to_string if_expr
    | Fn fn_expr ->
        Fn.to_string fn_expr
    | Call call_expr ->
        Call.to_string call_expr
end

and Identifier : NodeSig = MakeExpression (struct
  type t = {token: Token.t; value: String.t}

  let token_literal ({token; _} : t) : string = token.literal

  let to_string ({value; _} : t) : string = value
end)

and Boolean : NodeSig = MakeExpression (struct
  type t = {token: Token.t; value: Bool.t}

  let token_literal ({token; _} : t) : string = token.literal

  let to_string : t -> string = token_literal
end)

and Integer : NodeSig = MakeExpression (struct
  type t = {token: Token.t; value: Int.t}

  let token_literal ({token; _} : t) : string = token.literal

  let to_string : t -> string = token_literal
end)

and Prefix : NodeSig = MakeExpression (struct
  type t = {token: Token.t; operator: String.t; right: Expression.t}

  let token_literal ({token; _} : t) : string = token.literal

  let to_string (expression : t) : string = raise TODO
end)

and Infix : NodeSig = MakeExpression (struct
  type t = {token: Token.t; left: Expression.t; operator: String.t; right: Expression.t}

  let token_literal ({token; _} : t) : string = token.literal

  let to_string (expression : t) : string = raise TODO
end)

and If : NodeSig = MakeExpression (struct
  type t =
    {token: Token.t; condition: Expression.t; then_: BlockStatement.t; else_: BlockStatement.t}

  let token_literal ({token; _} : t) : string = token.literal

  let to_string (expression : t) : string = raise TODO
end)

and Fn : NodeSig = MakeExpression (struct
  type t = {token: Token.t; parameters: Identifier.t list; body: BlockStatement.t}

  let token_literal ({token; _} : t) : string = token.literal

  let to_string (expression : t) : string = raise TODO
end)

and Call : NodeSig = MakeExpression (struct
  type t = {token: Token.t; function_: Expression.t; arguments: Expression.t list}

  let token_literal ({token; _} : t) : string = token.literal

  let to_string (expression : t) : string = raise TODO
end)

and LetStatement : NodeSig = MakeStatement (struct
  type t = {token: Token.t; name: Identifier.t; value: String.t}

  let token_literal ({token; _} : t) : string = token.literal

  let to_string ({token; name; value} : t) : string =
    F.asprintf "%s %s = %s;" token.literal (Identifier.to_string name) value
end)

and ReturnStatement : NodeSig = MakeStatement (struct
  type t = {token: Token.t; value: Expression.t}

  let token_literal ({token; _} : t) : string = token.literal

  let to_string ({token; value} : t) : string =
    F.asprintf "%s %s;" token.literal @@ Expression.to_string value
end)

and ExpressionStatement : NodeSig = MakeStatement (struct
  type t = {token: Token.t; expression: Expression.t}

  let token_literal ({token; _} : t) : string = token.literal

  let to_string ({expression; _} : t) : string = Expression.to_string expression
end)

and BlockStatement : NodeSig = MakeStatement (struct
  type t = {token: Token.t; statements: Statement.t list}

  let token_literal ({token; _} : t) : string = token.literal

  let to_string ({statements; _} : t) : string =
    String.concat ~sep:"\n" @@ List.map ~f:Statement.to_string statements
end)

and Statement : NodeSig = struct
  type t =
    | Let of LetStatement.t
    | Return of ReturnStatement.t
    | Expression of ExpressionStatement.t
    | Block of BlockStatement.t

  let token_literal (statement : t) : string =
    match statement with
    | Let let_stmt ->
        LetStatement.token_literal let_stmt
    | Return rtn_stmt ->
        ReturnStatement.token_literal rtn_stmt
    | Expression expr_stmt ->
        ExpressionStatement.token_literal expr_stmt
    | Block block_stmt ->
        BlockStatement.token_literal block_stmt


  let to_string (statement : t) : string =
    match statement with
    | Let let_stmt ->
        LetStatement.to_string let_stmt
    | Return rtn_stmt ->
        ReturnStatement.to_string rtn_stmt
    | Expression expr_stmt ->
        ExpressionStatement.to_string expr_stmt
    | Block block_stmt ->
        BlockStatement.to_string block_stmt
end

module Program : NodeSig = struct
  type t = Statement.t List.t

  let token_literal (program : t) : string =
    match program with [] -> "" | statement :: _ -> Statement.token_literal statement


  let to_string (statements : t) : string =
    let ( >>| ) = List.( >>| ) in
    statements >>| Statement.to_string |> String.concat ~sep:"\n"
end
