exception TODO

type node = Statement of stmt | Expression of expr

and stmt = Let of let_stmt | Return of rtn_stmt | Expression of expr_stmt | Block of block_stmt

and expr =
  | Identifier of ident
  | Boolean of boolean
  | Integer of integer
  | Prefix of prefix
  | Infix of infix
  | If of if_expr
  | Fn of fun_literal
  | Call of call

let token_literal (node : node) : string = raise TODO

let to_string (node : node) : string = raise TODO

(* After being refactored.. It all becomes into module soup. :( *)
