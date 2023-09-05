open Token

exception TODO

type t = {input: String.t; position: Int.t; read_position: Int.t; ch: Char.t}

let default : t = {input= ""; position= 0; read_position= 0; ch= '\x00'}

(* ==================== Let's beat mutability ==================== *)

(* TODO: make .mli *)

(** Read a character starting from the lexer's current position. *)
let read_char (_lexer : t) : t * Char.t = raise TODO

(** Create a lexer that reads the input string from the beginning. *)
let of_string (input : string) : t = fst @@ read_char {default with input}

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

(** Get the next token which the lexer's current character begins. e.g. Get Operator.Equal or
    Operator.Assign depending on the peeked character. *)
let next_token (lexer : t) : t * Token.t =
  let whitespace_ignored = skip_whitespace lexer in
  match whitespace_ignored.ch with
  | '=' ->
      (* Either Operator.Equal or Operator.Assign *)
      let peeked_lexer, peeked_char = peek_char lexer in
      if Char.equal peeked_char '=' then
        (* Operator.Equal *)
        let char_read_lexer = fst @@ read_char peeked_lexer in
        ( char_read_lexer
        , { type_= Operator TokenType.Operator.Equal
          ; literal= Char.to_string peeked_char ^ Char.to_string char_read_lexer.ch } )
      else
        (* Operator.Assign *)
        ( peeked_lexer
        , {type_= Operator TokenType.Operator.Assign; literal= Char.to_string peeked_lexer.ch} )
  | _ ->
      raise TODO
