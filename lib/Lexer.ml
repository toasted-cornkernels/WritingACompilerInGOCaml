exception TODO

type t = {input: String.t; position: Int.t; read_position: Int.t; ch: Char.t}

let default : t = {input= ""; position= 0; read_position= 0; ch= '\x00'}

(* ==================== Let's beat mutability ==================== *)

(* TODO: make .mli *)

(** Read a character starting from the lexer's current position. *)
let read_char (_lexer : t) : t = raise TODO

(** Create a lexer that reads the input string from the beginning. *)
let of_string (input : string) : t = read_char {default with input}

(** Skip the whitespace, and the following ones, that the lexer may currently seeing. *)
let skip_whitespace (_lexer : t) : t = raise TODO

(** Peek a character without changing the lexer's current reading position. *)
let peek_char (_lexer : t) : Char.t = raise TODO

(** Read an identifier starting from the lexer's current position. *)
let read_identifier (_lexer : t) : t * String.t = raise TODO

(** Read a number starting from the lexer's current position. *)
let read_number (_lexer : t) : t * String.t = raise TODO

(** Is the character allowed in an identifier name? *)
let is_letter (char : Char.t) : bool = Char.is_alpha char || Char.equal char '_'

(** Get the next token which the lexer's current character begins. e.g. Get Operator.Equal or
    Operator.Assign depending on the peeked character. *)
let next_token (lexer : t) : t * Token.t =
  let open Token in
  let whitespace_ignored_lexer = skip_whitespace lexer in
  match whitespace_ignored_lexer.ch with
  | '=' ->
      if Char.equal '=' @@ peek_char whitespace_ignored_lexer then
        let char_read_lexer = read_char whitespace_ignored_lexer in
        ( read_char char_read_lexer
        , { type_= Operator TokenType.Operator.Equal
          ; literal= Char.to_string whitespace_ignored_lexer.ch ^ Char.to_string char_read_lexer.ch
          } )
      else
        ( read_char whitespace_ignored_lexer
        , { type_= Operator TokenType.Operator.Assign
          ; literal= Char.to_string whitespace_ignored_lexer.ch } )
  | '+' ->
      ( read_char whitespace_ignored_lexer
      , { type_= Operator TokenType.Operator.Plus
        ; literal= Char.to_string whitespace_ignored_lexer.ch } )
  | '-' ->
      ( read_char whitespace_ignored_lexer
      , { type_= Operator TokenType.Operator.Minus
        ; literal= Char.to_string whitespace_ignored_lexer.ch } )
  | '!' ->
      if Char.equal '=' @@ peek_char whitespace_ignored_lexer then
        let char_read_lexer = read_char whitespace_ignored_lexer in
        ( read_char char_read_lexer
        , { type_= Operator TokenType.Operator.NotEqual
          ; literal= Char.to_string whitespace_ignored_lexer.ch ^ Char.to_string char_read_lexer.ch
          } )
      else
        ( read_char whitespace_ignored_lexer
        , { type_= Operator TokenType.Operator.Bang
          ; literal= Char.to_string whitespace_ignored_lexer.ch } )
  | '/' ->
      ( read_char whitespace_ignored_lexer
      , { type_= Operator TokenType.Operator.Slash
        ; literal= Char.to_string whitespace_ignored_lexer.ch } )
  | '*' ->
      ( read_char whitespace_ignored_lexer
      , { type_= Operator TokenType.Operator.Asterisk
        ; literal= Char.to_string whitespace_ignored_lexer.ch } )
  | '<' ->
      ( read_char whitespace_ignored_lexer
      , { type_= Operator TokenType.Operator.LessThan
        ; literal= Char.to_string whitespace_ignored_lexer.ch } )
  | '>' ->
      ( read_char whitespace_ignored_lexer
      , { type_= Operator TokenType.Operator.GreaterThan
        ; literal= Char.to_string whitespace_ignored_lexer.ch } )
  | ';' ->
      ( read_char whitespace_ignored_lexer
      , { type_= Delimiter TokenType.Delimiter.Semicolon
        ; literal= Char.to_string whitespace_ignored_lexer.ch } )
  | ',' ->
      ( read_char whitespace_ignored_lexer
      , { type_= Delimiter TokenType.Delimiter.Comma
        ; literal= Char.to_string whitespace_ignored_lexer.ch } )
  | '{' ->
      ( read_char whitespace_ignored_lexer
      , { type_= Delimiter TokenType.Delimiter.LBrace
        ; literal= Char.to_string whitespace_ignored_lexer.ch } )
  | '}' ->
      ( read_char whitespace_ignored_lexer
      , { type_= Delimiter TokenType.Delimiter.RBrace
        ; literal= Char.to_string whitespace_ignored_lexer.ch } )
  | '\x00' ->
      (read_char whitespace_ignored_lexer, {type_= Meta TokenType.Meta.EOF; literal= ""})
  | _ ->
      if is_letter whitespace_ignored_lexer.ch then
        let identifier_read_lexer, identifier_read = read_identifier whitespace_ignored_lexer in
        (identifier_read_lexer, {type_= lookup_ident identifier_read; literal= identifier_read})
      else if Char.is_digit whitespace_ignored_lexer.ch then
        let number_read_lexer, number_read = read_number whitespace_ignored_lexer in
        (number_read_lexer, {type_= IdentLiteral TokenType.IdentLiteral.Int; literal= number_read})
      else
        ( whitespace_ignored_lexer
        , {type_= Meta TokenType.Meta.Illegal; literal= Char.to_string whitespace_ignored_lexer.ch}
        )
