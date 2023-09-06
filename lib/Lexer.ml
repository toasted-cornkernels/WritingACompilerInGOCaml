type t = {input: String.t; position: Int.t; read_position: Int.t; ch: Char.t}

let default : t = {input= ""; position= 0; read_position= 0; ch= '\x00'}

(** Consume a character of the input, shifting it to be the current char. *)
let read_char (lexer : t) : t =
  { lexer with
    ch=
      ( if Int.( >= ) lexer.read_position @@ String.length lexer.input then
          (* Can't consume any further *)
          '\x00'
        else String.get lexer.input lexer.read_position )
  ; position= lexer.read_position
  ; read_position= lexer.read_position + 1 }


(** Create a lexer that reads the input string from the beginning. *)
let of_string (input : String.t) : t = read_char {default with input}

(** Skip the whitespace, and the following ones, that the lexer may currently seeing. *)
let rec skip_whitespace (lexer : t) : t =
  match lexer.ch with ' ' | '\t' | '\n' | '\r' -> skip_whitespace lexer | _ -> lexer


(** Peek a character without changing the lexer's current reading position. *)
let peek_char (lexer : t) : Char.t =
  if Int.( >= ) lexer.read_position @@ String.length lexer.input then
    (* Can't consume any further *)
    '\x00'
  else String.get lexer.input lexer.read_position


(** Is the character allowed in an identifier name? *)
let is_letter (char : Char.t) : bool = Char.is_alpha char || Char.equal char '_'

(** Read an identifier starting from the lexer's current position. *)
let read_identifier (lexer : t) : t * String.t =
  let rec skip_all_letters (lexer_ : t) : t =
    if is_letter lexer_.ch then skip_all_letters (read_char lexer_) else lexer_
  in
  let skipped_letters_lexer = skip_all_letters lexer in
  ( skipped_letters_lexer
  , String.slice skipped_letters_lexer.input lexer.position skipped_letters_lexer.position )


(** Read a number starting from the lexer's current position. *)
let read_number (lexer : t) : t * String.t =
  let rec skip_all_numbers (lexer_ : t) : t =
    if Char.is_digit lexer_.ch then skip_all_numbers @@ read_char lexer_ else lexer_
  in
  let skipped_numbers_lexer = skip_all_numbers lexer in
  ( skipped_numbers_lexer
  , String.slice skipped_numbers_lexer.input lexer.position skipped_numbers_lexer.position )


(** Get the next token which the lexer's current character begins. e.g. Get Operator.Equal or
    Operator.Assign depending on the peeked character. *)
let next_token (lexer : t) : t * Token.t =
  let open Token in
  let open TokenType in
  let whitespace_ignored_lexer = skip_whitespace lexer in
  match whitespace_ignored_lexer.ch with
  | '=' ->
      if Char.equal '=' @@ peek_char whitespace_ignored_lexer then
        let char_read_lexer = read_char whitespace_ignored_lexer in
        ( read_char char_read_lexer
        , { type_= Operator Operator.Equal
          ; literal= Char.to_string whitespace_ignored_lexer.ch ^ Char.to_string char_read_lexer.ch
          } )
      else
        ( read_char whitespace_ignored_lexer
        , {type_= Operator Operator.Assign; literal= Char.to_string whitespace_ignored_lexer.ch} )
  | '+' ->
      ( read_char whitespace_ignored_lexer
      , {type_= Operator Operator.Plus; literal= Char.to_string whitespace_ignored_lexer.ch} )
  | '-' ->
      ( read_char whitespace_ignored_lexer
      , {type_= Operator Operator.Minus; literal= Char.to_string whitespace_ignored_lexer.ch} )
  | '!' ->
      if Char.equal '=' @@ peek_char whitespace_ignored_lexer then
        let char_read_lexer = read_char whitespace_ignored_lexer in
        ( read_char char_read_lexer
        , { type_= Operator Operator.NotEqual
          ; literal= Char.to_string whitespace_ignored_lexer.ch ^ Char.to_string char_read_lexer.ch
          } )
      else
        ( read_char whitespace_ignored_lexer
        , {type_= Operator Operator.Bang; literal= Char.to_string whitespace_ignored_lexer.ch} )
  | '/' ->
      ( read_char whitespace_ignored_lexer
      , {type_= Operator Operator.Slash; literal= Char.to_string whitespace_ignored_lexer.ch} )
  | '*' ->
      ( read_char whitespace_ignored_lexer
      , {type_= Operator Operator.Asterisk; literal= Char.to_string whitespace_ignored_lexer.ch} )
  | '<' ->
      ( read_char whitespace_ignored_lexer
      , {type_= Operator Operator.LessThan; literal= Char.to_string whitespace_ignored_lexer.ch} )
  | '>' ->
      ( read_char whitespace_ignored_lexer
      , {type_= Operator Operator.GreaterThan; literal= Char.to_string whitespace_ignored_lexer.ch}
      )
  | ';' ->
      ( read_char whitespace_ignored_lexer
      , {type_= Delimiter Delimiter.Semicolon; literal= Char.to_string whitespace_ignored_lexer.ch}
      )
  | ',' ->
      ( read_char whitespace_ignored_lexer
      , {type_= Delimiter Delimiter.Comma; literal= Char.to_string whitespace_ignored_lexer.ch} )
  | '{' ->
      ( read_char whitespace_ignored_lexer
      , {type_= Delimiter Delimiter.LBrace; literal= Char.to_string whitespace_ignored_lexer.ch} )
  | '}' ->
      ( read_char whitespace_ignored_lexer
      , {type_= Delimiter Delimiter.RBrace; literal= Char.to_string whitespace_ignored_lexer.ch} )
  | '\x00' ->
      (read_char whitespace_ignored_lexer, {type_= Meta Meta.EOF; literal= ""})
  | _ ->
      if is_letter whitespace_ignored_lexer.ch then
        let identifier_read_lexer, identifier_read = read_identifier whitespace_ignored_lexer in
        (identifier_read_lexer, {type_= lookup_ident identifier_read; literal= identifier_read})
      else if Char.is_digit whitespace_ignored_lexer.ch then
        let number_read_lexer, number_read = read_number whitespace_ignored_lexer in
        (number_read_lexer, {type_= IdentLiteral IdentLiteral.Int; literal= number_read})
      else
        ( whitespace_ignored_lexer
        , {type_= Meta Meta.Illegal; literal= Char.to_string whitespace_ignored_lexer.ch} )
