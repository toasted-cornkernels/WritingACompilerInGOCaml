type t = {lexer: Lexer.t; errors: String.t List.t; current_token: Token.t; peek_token: Token.t}

type precedence

type prefix_parser = t -> AST.Expression.t

type infix_parser = t -> AST.Expression.t -> AST.Expression.t

(* ==================== Prefix parsers ==================== *)

val parse_identifier : prefix_parser

val parse_integer : prefix_parser

val parse_bang : prefix_parser

val parse_minus : prefix_parser

val parse_true : prefix_parser

val parse_false : prefix_parser

val parse_lparen : prefix_parser

val parse_if : prefix_parser

val parse_function : prefix_parser

(* ==================== Infix parsers ==================== *)

val parse_plus : infix_parser

val parse_infix_minus : infix_parser

val parse_slash : infix_parser

val parse_asterisk : infix_parser

val parse_equal : infix_parser

val parse_not_equal : infix_parser

val parse_less_than : infix_parser

val parse_greater_than : infix_parser

val parse_infix_lparen : infix_parser

(* ==================== Expression and Statement parsers ==================== *)

val parse_expression : t -> precedence -> t * AST.Expression.t

val parse_let_statement : t -> t * AST.LetStatement.t

val parse_return_statement : t -> t * AST.LetStatement.t

val parse_expression_statement : t -> t * AST.ExpressionStatement.t

(* ==================== Whole program parser ==================== *)

val parse_program : t -> AST.Program.t
