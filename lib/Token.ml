module TokenType = struct
  module Meta = struct
    type t = Illegal | EOF

    let to_string = function Illegal -> "ILLEGAL" | EOF -> "EOF"
  end

  module IdentLiteral = struct
    type t = Ident | Int

    let to_string = function Ident -> "IDENT" | Int -> "INT"
  end

  module Operator = struct
    type t =
      | Assign
      | Plus
      | Minus
      | Bang
      | Asterisk
      | Slash
      | LessThan
      | GreaterThan
      | Equal
      | NotEqual

    let to_string = function
      | Assign ->
          "="
      | Plus ->
          "+"
      | Minus ->
          "-"
      | Bang ->
          "!"
      | Asterisk ->
          "*"
      | Slash ->
          "/"
      | LessThan ->
          "<"
      | GreaterThan ->
          ">"
      | Equal ->
          "=="
      | NotEqual ->
          "!="
  end

  module Delimiter = struct
    type t = Comma | Semicolon | LParen | RParen | LBrace | RBrace

    let to_string = function
      | Comma ->
          ","
      | Semicolon ->
          ";"
      | LParen ->
          "("
      | RParen ->
          ")"
      | LBrace ->
          "{"
      | RBrace ->
          "}"
  end

  module Keyword = struct
    type t = Function | Let | True | False | If | Else | Return

    let to_string = function
      | Function ->
          "FUNCTION"
      | Let ->
          "LET"
      | True ->
          "TRUE"
      | False ->
          "FALSE"
      | If ->
          "IF"
      | Else ->
          "ELSE"
      | Return ->
          "RETURN"
  end

  type t =
    | Meta of Meta.t
    | IdentLiteral of IdentLiteral.t
    | Operator of Operator.t
    | Delimiter of Delimiter.t
    | Keyword of Keyword.t

  let to_string = function
    | Meta meta ->
        Meta.to_string meta
    | IdentLiteral ident_literal ->
        IdentLiteral.to_string ident_literal
    | Operator operator ->
        Operator.to_string operator
    | Delimiter delimiter ->
        Delimiter.to_string delimiter
    | Keyword keyword ->
        Keyword.to_string keyword
end

module Keywords = struct
  module Map = Stdlib.Map
  include Map.Make (String)

  let keywords : TokenType.t t =
    let open TokenType in
    let open Keyword in
    empty
    |> add "fn" @@ Keyword Function
    |> add "let" @@ Keyword Let
    |> add "true" @@ Keyword True
    |> add "false" @@ Keyword False
    |> add "if" @@ Keyword If
    |> add "else" @@ Keyword Else
    |> add "return" @@ Keyword Return


  let lookup_ident (ident : String.t) : TokenType.t =
    match find_opt ident keywords with None -> IdentLiteral Ident | Some token -> token
end

module Token = struct
  type t = {type_: TokenType.t; literal: String.t}

  let of_char (token_type : TokenType.t) (char : Char.t) : t =
    {type_= token_type; literal= String.of_char char}
end
