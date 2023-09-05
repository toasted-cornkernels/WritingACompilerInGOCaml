open Token

exception TODO

type t = {input: String.t; position: Int.t; read_position: Int.t; ch: Char.t}

let default : t = {input= ""; position= 0; read_position= 0; ch= '\x00'}

(* ==================== Let's beat mutability ==================== *)

(* TODO: make .mli *)

(** Consume a character of the input, shifting it to be the current char. *)
let read_char (_lexer : t) : t = raise TODO

(** Create a lexer that reads the input string from the beginning. *)
let of_string (input : string) : t = read_char {default with input}

(** Get the next token which the lexer's current character begins. e.g. Get Operator.Equal or
    Operator.Assign depending on the peeked character. *)
let next_token (_lexer : t) : t * Token.t = raise TODO

(** Skip the whitespace, and the following ones, that the lexer may currently seeing. *)
let skip_whitespace (_lexer : t) : t = raise TODO

(** Peek a character without changing the lexer's current reading position. *)
let peek_char (_lexer : t) : t * Char.t = raise TODO

(** Read an identifier starting from the lexer's current position. *)
let read_identifier (_lexer : t) : t * String.t = raise TODO

(** Read a number starting from the lexer's current position. *)
let read_number (_lexer : t) : t * String.t = raise TODO

(** Is the character allowed in an identifier name? *)
let is_letter (char : Char.t) = Char.is_alpha char || Char.equal char '_'
