type t = {input: String.t; position: Int.t; read_position: Int.t; ch: Char.t}

val read_char : t -> t
(** Consume a character of the input, shifting it to be the current char. *)

val to_string : t -> String.t

val of_string : String.t -> t
(** Create a lexer that reads the input string from the beginning. *)

val skip_whitespace : t -> t
(** Skip the whitespace, and the following ones, that the lexer may currently seeing. *)

val peek_char : t -> Char.t
(** Peek a character without changing the lexer's current reading position. *)

val read_identifier : t -> t * String.t
(** Read an identifier starting from the lexer's current position. *)

val read_number : t -> t * String.t
(** Read a number starting from the lexer's current position. *)

val next_token : t -> t * Token.t
(** Get the next token which the lexer's current character begins. e.g. Get Operator.Equal or
    Operator.Assign depending on the peeked character. *)
