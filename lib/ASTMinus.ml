exception TODO

type let_statement = |

type return_statement = |

type block_statement = |

type expression_statement = |

type node = Statement of statement | Expression of expression

and statement =
  | LetStatment of let_statement
  | ReturnStatement of return_statement
  | ExpressionStatement of expression_statement
  | BlockStatement of block_statement

and expression =
  | Identifier of identifier
  | Boolean of boolean
  | Integer of integer
  | Prefix of prefix_expression
  | Infix of infix_expression
  | If of if_expression
  | Fn of function_literal
  | Call of call_expression

and identifier = {token: Token.t; value: String.t}

and boolean = |

and integer = |

and prefix_expression = {token: Token.t; operator: String.t; right: expression}

and infix_expression = |

and if_expression = |

and function_literal = |

and call_expression = |

let token_literal (node : node) : string = raise TODO

let to_string (node : node) : string = raise TODO
