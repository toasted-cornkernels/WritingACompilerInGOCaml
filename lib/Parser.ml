(** Recursive Descent (Pratt) Parser *)

open AST
open Lexer
open Token
open Token.TokenType
module F = Format

exception TODO

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


(** The parser type that contains:
    - A lexer for reading in tokens,
    - A list of errors for TODO,
    - The current token the parser is looking at, and
    - the next token to be consumed. *)
type t = {lexer: Lexer.t; errors: String.t List.t; current_token: Token.t; peek_token: Token.t}

type prefix_parser = t -> AST.Expression.t

type infix_parser = t -> AST.Expression.t -> AST.Expression.t

let parse_identifier : prefix_parser = fun _ -> raise TODO

let parse_integer : prefix_parser = fun _ -> raise TODO

let parse_bang : prefix_parser = fun _ -> raise TODO

let parse_minus : prefix_parser = fun _ -> raise TODO

let parse_true : prefix_parser = fun _ -> raise TODO

let parse_false : prefix_parser = fun _ -> raise TODO

let parse_lparen : prefix_parser = fun _ -> raise TODO

let parse_if : prefix_parser = fun _ -> raise TODO

let parse_function : prefix_parser = fun _ -> raise TODO

(** Determine which prefix parser to use when trampolining. *)
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


let parse_plus : infix_parser = fun _ _ -> raise TODO

let parse_infix_minus : infix_parser = fun _ _ -> raise TODO

let parse_slash : infix_parser = fun _ _ -> raise TODO

let parse_asterisk : infix_parser = fun _ _ -> raise TODO

let parse_equal : infix_parser = fun _ _ -> raise TODO

let parse_not_equal : infix_parser = fun _ _ -> raise TODO

let parse_less_than : infix_parser = fun _ _ -> raise TODO

let parse_greater_than : infix_parser = fun _ _ -> raise TODO

let parse_infix_lparen : infix_parser = fun _ _ -> raise TODO

(** Determine which infix parser to use when trampolining. *)
let dispatch_infix_parser (token_type : TokenType.t) : infix_parser =
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


let parse_expression (parser : t) (precendence : precedence) : t * Expression.t = raise TODO

(** Advance the given parser by one token, shifting both `current_token` and `peek_token`. *)
let next_token (parser : t) : t =
  let read_lexer, lexed_token = Lexer.next_token parser.lexer in
  {parser with current_token= parser.peek_token; peek_token= lexed_token; lexer= read_lexer}


(** An uninitialized parser. *)
let default : t =
  let eof = Meta.EOF in
  { lexer= Lexer.of_string ""
  ; errors= []
  ; current_token= {type_= Meta eof; literal= Meta.to_string eof}
  ; peek_token= {type_= Meta eof; literal= Meta.to_string eof} }


(* TODO: Don't expose the uninitialized parser to the interface. *)

(** Initialize a parser with a given lexer. *)
let of_lexer (lexer : Lexer.t) : t =
  (* Flush away the unneeded `Meta.EOF` token and fill current token and peek token.  *)
  {default with lexer} |> next_token |> next_token


(** Check if the current token this parser is looking at equals to a given token type. *)
let current_token_is (parser : t) (token_type : TokenType.t) : bool =
  TokenType.equal parser.current_token.type_ token_type


(** Check if the next token this parser is going to consume equals to a given token type. *)
let peek_token_is (parser : t) (token_type : TokenType.t) : bool =
  TokenType.equal parser.peek_token.type_ token_type


(** Append a wrong peeked token to the parser's error list. *)
let peek_error (parser : t) (token_type : Token.TokenType.t) : t =
  let msg =
    F.asprintf "Expected next token to be %s, got %s instead." (TokenType.to_string token_type)
      (TokenType.to_string parser.peek_token.type_)
  in
  (* TODO: Define a getter that reverses the errors list; just cons everything *)
  {parser with errors= parser.errors @ [msg]}


let expect_peek (parser : t) (token_type : TokenType.t) : t * bool =
  if peek_token_is parser token_type then (next_token parser, true)
  else (peek_error parser token_type, false)


let parse_let_statement (parser : t) : t * LetStatement.t =
  let let_token = parser.current_token in
  match expect_peek parser @@ IdentLiteral IdentLiteral.Ident with
  | ident_peeked_parser, true -> (
      let ident_consumed_parser = next_token ident_peeked_parser in
      let identifier_token : identifier =
        {token= parser.current_token; value= parser.current_token.literal}
      in
      match expect_peek ident_consumed_parser @@ Operator Operator.Assign with
      | assign_peeked_parser, true -> (
          let assign_consumed_parser = next_token assign_peeked_parser in
          match expect_peek assign_consumed_parser @@ Delimiter Delimiter.Semicolon with
          | semicolon_peeked_parser, true ->
              let let_statement : LetStatement.t =
                { token= let_token
                ; name= identifier_token
                ; value= Some (snd @@ parse_expression parser Lowest) }
              in
              (next_token semicolon_peeked_parser, let_statement)
          | error_parser, false ->
              raise TODO )
      | error_parser, false ->
          raise TODO )
  | error_parser, false ->
      raise TODO


let parse_return_statement (parser : t) : t * ReturnStatement.t =
  let return_token = parser.current_token in
  let return_token_consumed_parser = next_token parser in
  let expression_parsed_parser, return_value =
    parse_expression return_token_consumed_parser Lowest
  in
  (* TODO: account for empty returns *)
  let return_statement : ReturnStatement.t = {token= return_token; value= Some return_value} in
  if peek_token_is expression_parsed_parser @@ Delimiter Delimiter.Semicolon then
    (next_token expression_parsed_parser, return_statement)
  else (expression_parsed_parser, return_statement)


let parse_expression_statement (parser : t) : t * ExpressionStatement.t = raise TODO

let parse_statement (parser : t) : t * Statement.t =
  match parser.current_token.type_ with
  | Keyword Keyword.Let ->
      let let_statement_parsed_parser, let_statement = parse_let_statement parser in
      (let_statement_parsed_parser, Let let_statement)
  | Keyword Keyword.Return ->
      let return_statement_parsed_parser, return_statement = parse_return_statement parser in
      (return_statement_parsed_parser, Return return_statement)
  | _ ->
      let expression_statement_parsed_parser, expression_statement =
        parse_expression_statement parser
      in
      (expression_statement_parsed_parser, Expression expression_statement)


let parse_program (parser : t) : Program.t =
  let rec parse_program_inner (current_parser : t) (current_statements : Program.t) : Program.t =
    if TokenType.equal current_parser.current_token.type_ (Meta TokenType.Meta.EOF) then
      current_statements
    else
      let statement_parsed_parser, parsed_statement = parse_statement current_parser in
      parse_program_inner
        (next_token statement_parsed_parser)
        (parsed_statement :: current_statements)
  in
  parse_program_inner parser []
