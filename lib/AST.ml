exception TODO

module type NodeSig = sig
  type t

  val token_literal : t -> string

  val to_string : t -> string
end

module Expression (SubType : NodeSig) = struct
  type t = SubType.t

  let token_literal : t -> string = SubType.token_literal

  let to_string : t -> string = SubType.to_string
end

module Statement (SubType : NodeSig) = struct
  type t = SubType.t

  let token_literal : t -> string = SubType.token_literal

  let to_string : t -> string = SubType.to_string
end

module Expression = struct
  module Identifier = Expression (struct
    type t = {token: Token.t; value: String.t}

    let token_literal (expression : t) : string = raise TODO

    let to_string (expression : t) : string = raise TODO
  end)

  module Boolean = Expression (struct
    (* TODO: Complete this *)
    type t = |

    let token_literal (expression : t) : string = raise TODO

    let to_string (expression : t) : string = raise TODO
  end)

  module Integer = Expression (struct
    (* TODO: Complete this *)
    type t = |

    let token_literal (expression : t) : string = raise TODO

    let to_string (expression : t) : string = raise TODO
  end)

  module Prefix = Expression (struct
    (* TODO: Complete this *)
    type t = |

    let token_literal (expression : t) : string = raise TODO

    let to_string (expression : t) : string = raise TODO
  end)

  module Infix = Expression (struct
    (* TODO: Complete this *)
    type t = |

    let token_literal (expression : t) : string = raise TODO

    let to_string (expression : t) : string = raise TODO
  end)

  module If = Expression (struct
    (* TODO: Complete this *)
    type t = |

    let token_literal (expression : t) : string = raise TODO

    let to_string (expression : t) : string = raise TODO
  end)

  module Fn = Expression (struct
    (* TODO: Complete this *)
    type t = |

    let token_literal (expression : t) : string = raise TODO

    let to_string (expression : t) : string = raise TODO
  end)

  module Call = Expression (struct
    (* TODO: Complete this *)
    type t = |

    let token_literal (expression : t) : string = raise TODO

    let to_string (expression : t) : string = raise TODO
  end)

  type t =
    | Identifier of Identifier.t
    | Boolean of Boolean.t
    | Integer of Integer.t
    | Prefix of Prefix.t
    | Infix of Infix.t
    | If of If.t
    | Fn of Fn.t
    | Call of Call.t

  let token_literal (expression : t) : string = raise TODO

  let to_string (expression : t) : string = raise TODO
end

type ident = Expression.Identifier.t

module Statement = struct
  module Let = Statement (struct
    type t = {token: Token.t; name: Expression.Identifier.t; value: String.t}

    let token_literal (statement : t) : string = raise TODO

    let to_string (statement : t) : string = raise TODO
  end)

  module Return = Statement (struct
    (* TODO: Complete this *)
    type t = |

    let token_literal (statement : t) : string = raise TODO

    let to_string (statement : t) : string = raise TODO
  end)

  module Expression = Statement (struct
    (* TODO: Complete this *)
    type t = |

    let token_literal (statement : t) : string = raise TODO

    let to_string (statement : t) : string = raise TODO
  end)

  module Block = Statement (struct
    (* TODO: Complete this *)
    type t = |

    let token_literal (statement : t) : string = raise TODO

    let to_string (statement : t) : string = raise TODO
  end)

  type t = Let of Let.t | Return of Return.t | Expression of Expression.t | Block of Block.t

  let token_literal (statement : t) : string = raise TODO

  let to_string (statement : t) : string = raise TODO
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
