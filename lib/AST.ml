exception TODO

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

  let token_literal (expression : t) : string = raise TODO

  let to_string (expression : t) : string = raise TODO
end

and Identifier : NodeSig = MakeExpression (struct
  type t = {token: Token.t; value: String.t}

  let token_literal (expression : t) : string = raise TODO

  let to_string (expression : t) : string = raise TODO
end)

and Boolean : NodeSig = MakeExpression (struct
  type t = {token: Token.t; value: Bool.t}

  let token_literal (expression : t) : string = raise TODO

  let to_string (expression : t) : string = raise TODO
end)

and Integer : NodeSig = MakeExpression (struct
  type t = {token: Token.t; value: Int.t}

  let token_literal (expression : t) : string = raise TODO

  let to_string (expression : t) : string = raise TODO
end)

and Prefix : NodeSig = MakeExpression (struct
  type t = {token: Token.t; operator: String.t; right: Expression.t}

  let token_literal (expression : t) : string = raise TODO

  let to_string (expression : t) : string = raise TODO
end)

and Infix : NodeSig = MakeExpression (struct
  type t = {token: Token.t; left: Expression.t; operator: String.t; right: Expression.t}

  let token_literal (expression : t) : string = raise TODO

  let to_string (expression : t) : string = raise TODO
end)

and If : NodeSig = MakeExpression (struct
  type t =
    {token: Token.t; condition: Expression.t; then_: BlockStatement.t; else_: BlockStatement.t}

  let token_literal (expression : t) : string = raise TODO

  let to_string (expression : t) : string = raise TODO
end)

and Fn : NodeSig = MakeExpression (struct
  type t = {token: Token.t; parameters: Identifier.t list; body: BlockStatement.t}

  let token_literal (expression : t) : string = raise TODO

  let to_string (expression : t) : string = raise TODO
end)

and Call : NodeSig = MakeExpression (struct
  type t = {token: Token.t; function_: Expression.t; arguments: Expression.t list}

  let token_literal (expression : t) : string = raise TODO

  let to_string (expression : t) : string = raise TODO
end)

and LetStatement : NodeSig = MakeStatement (struct
  type t = {token: Token.t; name: Identifier.t; value: String.t}

  let token_literal (statement : t) : string = raise TODO

  let to_string (statement : t) : string = raise TODO
end)

and ReturnStatement : NodeSig = MakeStatement (struct
  type t = {token: Token.t; return_value: Expression.t}

  let token_literal (statement : t) : string = raise TODO

  let to_string (statement : t) : string = raise TODO
end)

and ExpressionStatement : NodeSig = MakeStatement (struct
  type t = {token: Token.t; expression: Expression.t}

  let token_literal (statement : t) : string = raise TODO

  let to_string (statement : t) : string = raise TODO
end)

and BlockStatement : NodeSig = MakeStatement (struct
  type t = {token: Token.t; statements: Statement.t list}

  let token_literal (statement : t) : string = raise TODO

  let to_string (statement : t) : string = raise TODO
end)

and Statement : NodeSig = struct
  type t =
    | Let of LetStatement.t
    | Return of ReturnStatement.t
    | Expression of ExpressionStatement.t
    | Block of BlockStatement.t

  let token_literal (statement : t) : string = raise TODO

  let to_string (statement : t) : string = raise TODO
end

module Program : NodeSig = struct
  type t = Statement.t List.t

  let token_literal (program : t) : string = raise TODO

  let to_string (program : t) : string = raise TODO
end

module Node : NodeSig = struct
  type t = MakeStatement of Statement.t | Expression of Expression.t | Program of Program.t

  let token_literal (node : t) : string = raise TODO

  let to_string (node : t) : string = raise TODO
end
