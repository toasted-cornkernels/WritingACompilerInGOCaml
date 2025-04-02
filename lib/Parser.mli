type t = {lexer: Lexer.t; errors: String.t List.t; current_token: Token.t; peek_token: Token.t}

type precedence

type prefix_parser = t -> t * AST.Expression.t option

type infix_parser = t -> AST.Expression.t -> t * AST.Expression.t option

val next_token : t -> t

val of_lexer : Lexer.t -> t

(* ==================== Prefix parsers ==================== *)

val parse_identifier : prefix_parser

val parse_integer : prefix_parser

val parse_prefix_expression : prefix_parser
(** Parses these *prefix* operator tokens:
    - Operator.Bang
    - Operator.Minus *)

val parse_boolean : prefix_parser
(** Parses these *prefix* operator tokens:
    - Operator.True
    - Operator.False *)

val parse_grouped_expression : prefix_parser
(** Parses Delimiter.LParen occuring in the prefix position, i.e. grouped expression to indicate evaluation order. *)

val parse_if : prefix_parser

val parse_function : prefix_parser

(* ==================== Infix parsers ==================== *)

val parse_infix_expression : infix_parser
(** Parses these infix operator tokens:
    - Operator.Plus,
    - Operator.Minus,
    - Operator.Slash,
    - Operator.Equal,
    - Operator.NotEqual,
    - Operator.LessThan, and
    - Operator.GreaterThan. *)

val parse_call_expression : infix_parser
(** Parses Delimiter.LParen occurring in the infix position, i.e. argument list of a function call. *)

(* ==================== Expression and Statement parsers ==================== *)

val parse_expression : t -> precedence -> t * AST.Expression.t option

val parse_let_statement : t -> t * AST.LetStatement.t option

val parse_return_statement : t -> t * AST.ReturnStatement.t option

val parse_expression_statement : t -> t * AST.ExpressionStatement.t option

(* ==================== Whole program parser ==================== *)

val parse_program : t -> AST.Program.t
