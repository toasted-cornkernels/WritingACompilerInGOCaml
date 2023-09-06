type t

val read_char : t -> t

val of_string : String.t -> t

val skip_whitespace : t -> t

val peek_char : t -> Char.t

val read_identifier : t -> t * String.t

val read_number : t -> t * String.t

val next_token : t -> t * Token.t
