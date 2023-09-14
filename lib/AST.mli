exception TODO

module Statement : sig
  module Let : sig
    type t

    val token_literal : t -> string

    val to_string : t -> string
  end

  module Return : sig
    type t

    val token_literal : t -> string

    val to_string : t -> string
  end

  module Expression : sig
    type t

    val token_literal : t -> string

    val to_string : t -> string
  end

  module Block : sig
    type t

    val token_literal : t -> string

    val to_string : t -> string
  end

  type t = Let of Let.t | Return of Return.t | Expression of Expression.t | Block of Block.t

  val token_literal : t -> string

  val to_string : t -> string
end

module Expression : sig
  module Identifier : sig
    type t

    val token_literal : t -> string

    val to_string : t -> string
  end

  module Boolean : sig
    type t

    val token_literal : t -> string

    val to_string : t -> string
  end

  module Integer : sig
    type t

    val token_literal : t -> string

    val to_string : t -> string
  end

  module Prefix : sig
    type t

    val token_literal : t -> string

    val to_string : t -> string
  end

  module Infix : sig
    type t

    val token_literal : t -> string

    val to_string : t -> string
  end

  module If : sig
    type t

    val token_literal : t -> string

    val to_string : t -> string
  end

  module Fn : sig
    type t

    val token_literal : t -> string

    val to_string : t -> string
  end

  module Call : sig
    type t

    val token_literal : t -> string

    val to_string : t -> string
  end

  type t =
    | Identifier of Identifier.t
    | Boolean of Boolean.t
    | Integer of Integer.t
    | Prefix of Prefix.t
    | Infix of Infix.t
    | If of If.t
    | Fn of Fn.t
    | Call of Call.t

  val token_literal : t -> string

  val to_string : t -> string
end

module Program : sig end

module Node : sig end
