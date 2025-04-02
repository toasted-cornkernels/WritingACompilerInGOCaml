open Token
open Token.TokenType

exception ParserError of string

let check_parser_error (parser : Parser.t) : unit =
  if not @@ List.is_empty parser.errors then
    raise
      (ParserError
         (F.sprintf "Parser has %d errors: %s" (List.length parser.errors)
            (List.fold_left parser.errors ~init:"" ~f:(fun acc elem -> acc ^ elem ^ "\n")) ) )


module LetStatementTest = struct
  let input = {|let x = 5;
               let y = 10;
               let foobar = 838383;|}

  let parser = input |> Lexer.of_string |> Parser.of_lexer

  let program = Parser.parse_program parser

  (* TODO: Port this to Alcotest (string list), while not forgetting to call check_parser_error on the final parser. *)

  (** Test if a given let statement has an identifier of a given name. *)
  let test_let_statement ((stmt, name) : AST.Statement.t * String.t) : Unit.t =
    match stmt with
    | Let let_stmt ->
        Alcotest.check Alcotest.string "Token name" let_stmt.token.literal "let" ;
        (* The name can be accessed either via the AST node's identifier value or the underlying token's literal value. *)
        Alcotest.check Alcotest.string "Name test via AST node's identifier value"
          let_stmt.name.value name ;
        assert (String.equal let_stmt.name.value name) ;
        Alcotest.check Alcotest.string "Name test via underlying token's literal value"
          let_stmt.name.token.literal name
    | _ ->
        raise @@ Invalid_argument "Well that's not an expression statement."


  let test_let_statement () =
    List.iter ~f:test_let_statement @@ List.zip_exn program ["x"; "y"; "foobar"]

  (* Fails for now, since parse_program's not fully cooked yet *)
end

module ReturnStatementTest = struct
  let input = {|return 5;
               return 10;
               return 993322;|}

  let parser = input |> Lexer.of_string |> Parser.of_lexer

  let program = Parser.parse_program parser

  let test_return_statement () =
    Alcotest.check' Alcotest.int ~msg:"There should be three statements." ~expected:1
      ~actual:(List.length program) ;
    List.iter program ~f:(fun statement ->
        match statement with
        | Return return_statement ->
            Alcotest.check' Alcotest.string ~msg:"The token should be \"return\"."
              ~expected:"return" ~actual:return_statement.token.literal
        | other ->
            Alcotest.failf "Expected return statement, got %s." (AST.Statement.to_string other) )
end

module ExpressionStatementTest = struct
  (* let input =  *)
end

let test_suite = [("Let Statement", `Quick, LetStatementTest.test_let_statement)]

let () = Alcotest.run "Parser test" [("Parser test", test_suite)]
