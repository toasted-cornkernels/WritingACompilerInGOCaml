(* Recursive Descent (Pratt) Parser *)

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


type t = {lexer: Lexer.t; errors: String.t List.t; current_token: Token.t; peek_token: Token.t}

let default : t =
  let open Token.TokenType in
  let eof = Meta.EOF in
  { lexer= Lexer.of_string ""
  ; errors= []
  ; current_token= {type_= Meta eof; literal= Meta.to_string eof}
  ; peek_token= {type_= Meta eof; literal= Meta.to_string eof} }


exception TODO

type prefix_parser = t -> AST.Expression.t

type infix_parser = t -> AST.Expression.t -> AST.Expression.t

let parse_identifier : prefix_parser = raise TODO

let parse_integer : prefix_parser = raise TODO

let parse_bang : prefix_parser = raise TODO

let parse_minus : prefix_parser = raise TODO

let parse_true : prefix_parser = raise TODO

let parse_false : prefix_parser = raise TODO

let parse_lparen : prefix_parser = raise TODO

let parse_if : prefix_parser = raise TODO

let parse_function : prefix_parser = raise TODO

let parse_plus : infix_parser = raise TODO

let parse_infix_minus : infix_parser = raise TODO

let parse_slash : infix_parser = raise TODO

let parse_asterisk : infix_parser = raise TODO

let parse_equal : infix_parser = raise TODO

let parse_not_equal : infix_parser = raise TODO

let parse_less_than : infix_parser = raise TODO

let parse_greater_than : infix_parser = raise TODO

let parse_infix_lparen : infix_parser = raise TODO

let dispatch_prefix_parser (token_type : TokenType.t) : prefix_parser =
  let open TokenType in
  match token_type with
  | IdentLiteral IdentLiteral.Ident ->
      parse_identifier
  | IdentLiteral IdentLiteral.Int ->
      parse_integer
  | Operator Operator.Bang ->
      parse_bang
  | Operator Operator.Minus ->
      parse_minus
  | Keyword Keyword.True ->
      parse_true
  | Keyword Keyword.False ->
      parse_false
  | Delimiter Delimiter.LParen ->
      parse_lparen
  | Keyword Keyword.If ->
      parse_if
  | Keyword Keyword.Function ->
      parse_function
  | _ ->
      raise TODO


let dispatch_infix_parser (token_type : TokenType.t) : infix_parser =
  let open TokenType in
  match token_type with
  | Operator Operator.Plus ->
      parse_plus
  | Operator Operator.Minus ->
      parse_infix_minus
  | Operator Operator.Asterisk ->
      parse_asterisk
  | Operator Operator.Slash ->
      parse_slash
  | Operator Operator.Equal ->
      parse_equal
  | Operator Operator.NotEqual ->
      parse_not_equal
  | Operator Operator.LessThan ->
      parse_less_than
  | Operator Operator.GreaterThan ->
      parse_greater_than
  | Delimiter Delimiter.LParen ->
      parse_infix_lparen
  | _ ->
      raise TODO


let dispatch_infix_parse_function = raise TODO
