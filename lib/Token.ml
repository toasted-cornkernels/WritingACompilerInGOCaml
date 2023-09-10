module F = Format

module TokenType = struct
  module Meta = struct
    type t = Illegal | EOF [@@deriving equal]

    let to_string = function Illegal -> "ILLEGAL" | EOF -> "EOF"

    let pp (fmt : F.formatter) (meta : t) = F.fprintf fmt "%s" @@ to_string meta
  end

  module IdentLiteral = struct
    type t = Ident | Int [@@deriving equal]

    let to_string = function Ident -> "IDENT" | Int -> "INT"

    let pp (fmt : F.formatter) (ident_literal : t) = F.fprintf fmt "%s" @@ to_string ident_literal
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
    [@@deriving equal]

    let pp (fmt : F.formatter) (operator : t) =
      let string =
        match operator with
        | Assign ->
            "Assign"
        | Plus ->
            "Plus"
        | Minus ->
            "Minus"
        | Bang ->
            "Bang"
        | Asterisk ->
            "Asterisk"
        | Slash ->
            "Slash"
        | LessThan ->
            "LessThan"
        | GreaterThan ->
            "GreaterThan"
        | Equal ->
            "Equal"
        | NotEqual ->
            "NotEqual"
      in
      F.fprintf fmt "%s" string


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
    type t = Comma | Semicolon | LParen | RParen | LBrace | RBrace [@@deriving equal]

    let pp (fmt : F.formatter) (delimiter : t) =
      let string =
        match delimiter with
        | Comma ->
            "Comma"
        | Semicolon ->
            "Semicolon"
        | LParen ->
            "LParen"
        | RParen ->
            "RParen"
        | LBrace ->
            "LBrace"
        | RBrace ->
            "RBrace"
      in
      F.fprintf fmt "%s" string


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
    type t = Function | Let | True | False | If | Else | Return [@@deriving equal]

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


    let pp (fmt : F.formatter) (keyword : t) = F.fprintf fmt "%s" @@ to_string keyword
  end

  type t =
    | Meta of Meta.t
    | IdentLiteral of IdentLiteral.t
    | Operator of Operator.t
    | Delimiter of Delimiter.t
    | Keyword of Keyword.t
  [@@deriving equal]

  let pp (fmt : F.formatter) (token_type : t) =
    match token_type with
    | Meta meta ->
        F.fprintf fmt "%a" Meta.pp meta
    | IdentLiteral ident_literal ->
        F.fprintf fmt "%a" IdentLiteral.pp ident_literal
    | Operator operator ->
        F.fprintf fmt "%a" Operator.pp operator
    | Delimiter delimiter ->
        F.fprintf fmt "%a" Delimiter.pp delimiter
    | Keyword keyword ->
        F.fprintf fmt "%a" Keyword.pp keyword


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
end

type t = {type_: TokenType.t; literal: String.t} [@@deriving equal]

let to_string ({type_; literal} : t) : string =
  let module F = Format in
  F.asprintf "{type_= %s; literal= %s}" (TokenType.to_string type_) literal


let pp (fmt : F.formatter) ({type_; literal} : t) =
  F.fprintf fmt "[Token type= %a, literal= %s]" TokenType.pp type_ literal


let of_char (token_type : TokenType.t) (char : Char.t) : t =
  {type_= token_type; literal= String.of_char char}


let lookup_ident (ident : String.t) : TokenType.t =
  match Keywords.find_opt ident Keywords.keywords with
  | None ->
      IdentLiteral Ident
  | Some token ->
      token


let is_ident (ident : String.t) : bool = Option.is_some @@ Keywords.find_opt ident Keywords.keywords
