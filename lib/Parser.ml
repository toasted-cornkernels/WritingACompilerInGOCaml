(** Recursive Descent (Pratt) Parser *)

open AST
open Token
open Token.TokenType
module F = Format

exception TODO

exception No_Prefix_Parser_Available of TokenType.t

exception No_Infix_Parser_Available of TokenType.t

type precedence =
  | Lowest
  | Equals (* == *)
  | LessGreater (* < or > *)
  | Sum (* + *)
  | Product (* * *)
  | Prefix (* -EXP or !EXP *)
  | Call (* F(X) *)
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
    - A list of errors for parsing error reporting,
    - The current token the parser is looking at, and
    - the next token to be consumed. *)
type t = {lexer: Lexer.t; errors: String.t List.t; current_token: Token.t; peek_token: Token.t}

exception TODO_Parser_Error_Handle of t

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


(** Initialize a parser with a given lexer. *)
let of_lexer (lexer : Lexer.t) : t =
  (* Flush away the unneeded `Meta.EOF` token and fill current token and peek token. *)
  {default with lexer} |> next_token |> next_token


(** Check if the current token this parser is looking at equals to a given token type. *)
let current_token_is (parser : t) (token_type : TokenType.t) : bool =
  TokenType.equal parser.current_token.type_ token_type


(** Check if the next token this parser is going to consume equals to a given token type. *)
let peek_token_is (parser : t) (token_type : TokenType.t) : bool =
  TokenType.equal parser.peek_token.type_ token_type


(** Append a wrong peeked token to the parser's error list. *)
let peek_error (parser : t) (token_type : Token.TokenType.t) : t =
  let error_message =
    F.asprintf "Expected next token to be %s, got %s instead." (TokenType.to_string token_type)
      (TokenType.to_string parser.peek_token.type_)
  in
  {parser with errors= error_message :: parser.errors}


(** If the parser's peeked token has the expected token type, then advance the parser. Otherwise, do
    not advance it and instead add an new entry in the parser error. *)
let expect_peek (parser : t) (token_type : TokenType.t) : t * bool =
  if peek_token_is parser token_type then (next_token parser, true)
  else (peek_error parser token_type, false)


let errors (parser : t) : string list = List.rev parser.errors

(* ==================== Expression Parsers ==================== *)

type prefix_parser = t -> t * AST.Expression.t option

type infix_parser = t -> AST.Expression.t -> t * AST.Expression.t option

let rec parse_expression (parser : t) (precedence : precedence) : t * Expression.t option =
  let infix_parser = dispatch_prefix_parser parser.current_token.type_ in
  let lhs_expression = infix_parser parser in
  raise TODO


(** Determine which prefix parser to use when trampolining. *)
and dispatch_prefix_parser (token_type : TokenType.t) : prefix_parser =
  let open TokenType in
  match token_type with
  | IdentLiteral IdentLiteral.Ident ->
      parse_identifier
  | IdentLiteral IdentLiteral.Int ->
      parse_integer
  | Operator Operator.Bang | Operator Operator.Minus ->
      parse_prefix_expression
  | Keyword Keyword.True | Keyword Keyword.False ->
      parse_boolean
  | Delimiter Delimiter.LParen ->
      parse_grouped_expression
  | Keyword Keyword.If ->
      parse_if
  | Keyword Keyword.Function ->
      parse_function
  | _ ->
      raise @@ No_Prefix_Parser_Available token_type


and parse_identifier : prefix_parser =
 fun parser ->
  (* Uhh.. shouldn't we advance the parser here by one token? *)
  (parser, Some (Identifier {token= parser.current_token; value= parser.current_token.literal}))


and parse_integer : prefix_parser =
 fun parser ->
  match Int.of_string_opt parser.current_token.literal with
  | Some value ->
      (* Uhh.. shouldn't we advance the parser here by one token? *)
      (parser, Some (Integer {token= parser.current_token; value}))
  | None ->
      let error_message =
        F.asprintf "Could not parse %s as integer." parser.current_token.literal
      in
      ({parser with errors= error_message :: parser.errors}, None)


and parse_prefix_expression : prefix_parser =
 fun parser ->
  let current_operator_consumed_parser = next_token parser in
  let right_expression_parsed_parser, expression_opt =
    parse_expression current_operator_consumed_parser Prefix
  in
  match expression_opt with
  | Some expression ->
      ( right_expression_parsed_parser
      , Some
          (Prefix
             {token= parser.current_token; operator= parser.current_token.literal; right= expression}
          ) )
  | None ->
      (right_expression_parsed_parser, None)


and parse_boolean : prefix_parser =
 fun parser ->
  let current_token = parser.current_token in
  match parser.current_token.type_ with
  | Keyword True ->
      (parser, Some (Boolean {token= current_token; value= true}))
  | Keyword False ->
      (parser, Some (Boolean {token= current_token; value= false}))
  | _ ->
      let error_message =
        F.asprintf "Could not parse token %s as a boolean" parser.current_token.literal
      in
      ({parser with errors= error_message :: parser.errors}, None)


and parse_grouped_expression : prefix_parser =
 fun parser ->
  let expression_parsed_parser, expression_opt = parse_expression parser Lowest in
  match expect_peek expression_parsed_parser (Delimiter RParen) with
  | rparen_consumed_parser, true ->
      (rparen_consumed_parser, expression_opt)
  | error_parser, false ->
      (error_parser, None)


(* What a beep-ing abomination *)
and parse_if : prefix_parser =
 fun parser ->
  let if_token = parser.current_token in
  match expect_peek parser (Delimiter LParen) with
  | lparen_consumed_parser, true -> (
      let expression_parsed_parser, condition_expression_opt =
        parse_expression lparen_consumed_parser Lowest
      in
      match condition_expression_opt with
      | Some condition_expression -> (
        match expect_peek expression_parsed_parser (Delimiter RParen) with
        | rparen_consumed_parser, true -> (
          match expect_peek rparen_consumed_parser (Delimiter LBrace) with
          | lbrace_consumed_parser, true -> (
              let consequence_block_parsed_parser, consequence_opt =
                parse_block_statement lbrace_consumed_parser
              in
              match consequence_opt with
              | Some consequence -> (
                match peek_token_is consequence_block_parsed_parser (Keyword Else) with
                | true -> (
                  match expect_peek consequence_block_parsed_parser (Delimiter LBrace) with
                  | lbrace_consumed_parser, true ->
                      let alternative_block_parsed_parser, alternative_opt =
                        parse_block_statement lbrace_consumed_parser
                      in
                      ( alternative_block_parsed_parser
                      , Some
                          (If
                             { token= if_token
                             ; condition= condition_expression
                             ; then_= consequence
                             ; else_= alternative_opt } ) )
                  | error_parser, false ->
                      (error_parser, None) )
                | false ->
                    ( consequence_block_parsed_parser
                    , Some
                        (If
                           { token= if_token
                           ; condition= condition_expression
                           ; then_= consequence
                           ; else_= None } ) ) )
              | None ->
                  (consequence_block_parsed_parser, None) )
          | error_parser, false ->
              (error_parser, None) )
        | error_parser, false ->
            (error_parser, None) )
      | None ->
          (expression_parsed_parser, None) )
  | error_parser, false ->
      (error_parser, None)


and parse_function : prefix_parser = fun _ -> raise TODO

(** Determine which infix parser to use when trampolining. *)
and dispatch_infix_parser (token_type : TokenType.t) : infix_parser =
  match token_type with
  | Operator Operator.Plus
  | Operator Operator.Minus
  | Operator Operator.Asterisk
  | Operator Operator.Slash
  | Operator Operator.Equal
  | Operator Operator.NotEqual
  | Operator Operator.LessThan
  | Operator Operator.GreaterThan ->
      parse_infix_expression
  | Delimiter Delimiter.LParen ->
      parse_call_expression
  | _ ->
      raise @@ No_Infix_Parser_Available token_type


and parse_infix_expression : infix_parser = fun parser lhs -> raise TODO

and parse_call_expression : infix_parser = fun parser function_ -> raise TODO

(* ==================== Statement Parsers ==================== *)

(* "consumed" == "current token is" *)

and parse_let_statement (parser : t) : t * LetStatement.t option =
  let let_token = parser.current_token in
  match expect_peek parser @@ IdentLiteral IdentLiteral.Ident with
  | ident_consumed_parser, true -> (
      let identifier_token : identifier =
        {token= parser.current_token; value= parser.current_token.literal}
      in
      match expect_peek ident_consumed_parser @@ Operator Operator.Assign with
      | assign_consumed_parser, true -> (
          let expression_consumed_parser = next_token assign_consumed_parser in
          let expression_parsed_parser, expression_opt =
            parse_expression expression_consumed_parser Lowest
          in
          match peek_token_is expression_parsed_parser @@ Delimiter Delimiter.Semicolon with
          | true ->
              let let_statement : LetStatement.t =
                {token= let_token; name= identifier_token; value= expression_opt}
              in
              (next_token expression_parsed_parser, Some let_statement)
          | false ->
              let let_statement : LetStatement.t =
                {token= let_token; name= identifier_token; value= expression_opt}
              in
              (expression_parsed_parser, Some let_statement) )
      | error_parser, false ->
          (error_parser, None) )
  | error_parser, false ->
      (error_parser, None)


and parse_return_statement (parser : t) : t * ReturnStatement.t option =
  let return_token = parser.current_token in
  let return_token_consumed_parser = next_token parser in
  let expression_parsed_parser, return_value_opt =
    parse_expression return_token_consumed_parser Lowest
  in
  (* TODO: account for empty returns *)
  let return_statement : ReturnStatement.t = {token= return_token; value= return_value_opt} in
  if peek_token_is expression_parsed_parser @@ Delimiter Delimiter.Semicolon then
    (next_token expression_parsed_parser, Some return_statement)
  else (expression_parsed_parser, Some return_statement)


and parse_expression_statement (parser : t) : t * ExpressionStatement.t option =
  let current_token = parser.current_token in
  let expression_parsed_parser, expression_opt = parse_expression parser Lowest in
  if peek_token_is expression_parsed_parser (Delimiter Delimiter.Semicolon) then
    (next_token expression_parsed_parser, Some {token= current_token; expression= expression_opt})
  else (* Make semicolons in an expression statement optional, for ease of input in the REPL. *)
    (expression_parsed_parser, Some {token= current_token; expression= expression_opt})


and parse_statement (parser : t) : t * Statement.t option =
  match parser.current_token.type_ with
  | Keyword Keyword.Let -> (
      let let_statement_parsed_parser, let_statement_opt = parse_let_statement parser in
      match let_statement_opt with
      | Some let_statement ->
          (let_statement_parsed_parser, Some (Let let_statement))
      | None ->
          (let_statement_parsed_parser, None) )
  | Keyword Keyword.Return -> (
      let return_statement_parsed_parser, return_statement_opt = parse_return_statement parser in
      match return_statement_opt with
      | Some return_statement ->
          (return_statement_parsed_parser, Some (Return return_statement))
      | None ->
          (return_statement_parsed_parser, None) )
  | _ -> (
      let expression_statement_parsed_parser, expression_statement_opt =
        parse_expression_statement parser
      in
      match expression_statement_opt with
      | Some expression_statement ->
          (expression_statement_parsed_parser, Some (Expression expression_statement))
      | None ->
          (expression_statement_parsed_parser, None) )


and parse_block_statement (parser : t) : t * BlockStatement.t option =
  let lbrace_token = parser.current_token in
  let rec inner (current_statements : Statement.t list) (current_parser : t) =
    (* Normally the loop should halt on hitting an RBrace, but the LBrace may not be properly closed. *)
    if
      current_token_is current_parser (Delimiter RBrace)
      || current_token_is current_parser (Meta EOF)
    then (current_parser, List.rev current_statements)
    else
      let statement_parsed_parser, statement_opt = parse_statement current_parser in
      (* QUESTION: Why do we have to advance the parser when looping? *)
      match statement_opt with
      | Some statement ->
          inner (statement :: current_statements) (next_token statement_parsed_parser)
      | None ->
          inner current_statements (next_token statement_parsed_parser)
  in
  let statements_parsed_parser, parsed_statements = inner [] parser in
  (statements_parsed_parser, Some {token= lbrace_token; statements= parsed_statements})


let parse_program (parser : t) : Program.t =
  let rec parse_program_inner (current_parser : t) (current_statements : Program.t) : Program.t =
    if TokenType.equal current_parser.current_token.type_ (Meta TokenType.Meta.EOF) then
      current_statements
    else
      let statement_parsed_parser, parsed_statement_opt = parse_statement current_parser in
      match parsed_statement_opt with
      | Some parsed_statement ->
          parse_program_inner
            (next_token statement_parsed_parser)
            (parsed_statement :: current_statements)
      | None ->
          parse_program_inner (next_token statement_parsed_parser) current_statements
  in
  parse_program_inner parser []
