exception TODO

module type NodeSig = sig
  type t

  val token_literal : t -> string

  val to_string : t -> string
end

module Statement : NodeSig = struct
  module Let : NodeSig = struct
    type t = |

    let token_literal (statement : t) : string = raise TODO

    let to_string (statement : t) : string = raise TODO
  end

  module Return : NodeSig = struct
    type t = |

    let token_literal (statement : t) : string = raise TODO

    let to_string (statement : t) : string = raise TODO
  end

  module Expression : NodeSig = struct
    type t = |

    let token_literal (statement : t) : string = raise TODO

    let to_string (statement : t) : string = raise TODO
  end

  module Block : NodeSig = struct
    type t = |

    let token_literal (statement : t) : string = raise TODO

    let to_string (statement : t) : string = raise TODO
  end

  type t = Let of Let.t | Return of Return.t | Expression of Expression.t | Block of Block.t

  let token_literal (statement : t) : string = raise TODO

  let to_string (statement : t) : string = raise TODO
end

(* and expr = *)
(*   | Identifier of ident *)
(*   | Boolean of boolean *)
(*   | Integer of integer *)
(*   | Prefix of prefix *)
(*   | Infix of infix *)
(*   | If of if_expr *)
(*   | Fn of fun_literal *)
(*   | Call of call *)

module Expression : NodeSig = struct
  (* TODO *)
  type t = |

  let token_literal (expression : t) : string = raise TODO

  let to_string (expression : t) : string = raise TODO
end

module Program : NodeSig = struct
  type t = Statement.t List.t

  let token_literal (program : t) : string = raise TODO

  let to_string (program : t) : string = raise TODO
end

module Node : NodeSig = struct
  type t = Statement of Statement.t | Expression of Expression.t | Program of Program.t

  let token_literal (node : t) : string = raise TODO

  let to_string (node : t) : string = raise TODO
end
