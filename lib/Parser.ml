open AST
open Lexer
open Token
module F = Format

type precedence = Lowest | Equals | LessGreater | Sum | Product | Prefix | Call
[@@deriving compare, equal]

let precedence_table : (TokenType.t, precedence) Stdlib.Hashtbl.t =
  let open Stdlib in
  let open TokenType in
  let tbl = Hashtbl.create 777 in
  Hashtbl.add tbl (Operator Operator.Equal) Equals ;
  Hashtbl.add tbl (Operator Operator.NotEqual) Equals ;
  Hashtbl.add tbl (Operator Operator.LessThan) LessGreater ;
  Hashtbl.add tbl (Operator Operator.GreaterThan) LessGreater ;
  Hashtbl.add tbl (Operator Operator.Plus) Sum ;
  Hashtbl.add tbl (Operator Operator.Minus) Sum ;
  Hashtbl.add tbl (Operator Operator.Asterisk) Product ;
  Hashtbl.add tbl (Operator Operator.Slash) Product ;
  Hashtbl.add tbl (Delimiter Delimiter.LParen) Call ;
  tbl
