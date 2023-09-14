module Expression : sig
  type t

  val token_literal : t -> string

  val to_string : t -> string
end

module Statement : sig
  type t

  val token_literal : t -> string

  val to_string : t -> string
end

module Program : sig
  type t

  val token_literal : t -> string

  val to_string : t -> string
end
