exception TODO

module type NodeSig = sig
  type t

  val token_literal : t -> string

  val to_string : t -> string
end

module Expression : NodeSig

module Statement : NodeSig

module Program : NodeSig

module Node : NodeSig
