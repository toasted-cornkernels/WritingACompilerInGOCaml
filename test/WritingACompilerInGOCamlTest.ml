exception TODO

module LexerTest = struct
  open Token.TokenType

  let input =
    {|let five = 5;
     let ten = 10;

     let add = fn(x, y) {
       x + y;
     };

     let result = add(five, ten);
     !-/*5;
     5 < 10 > 5;

     if (5 < 10) {
       return true;
     } else {
       return false;
     }

     10 == 10;
     10 != 9;|}


  let expected : Token.t list =
    [ {type_= Keyword Keyword.Let; literal= "let"}
    ; {type_= IdentLiteral IdentLiteral.Ident; literal= "five"}
    ; {type_= Operator Operator.Assign; literal= "="}
    ; {type_= IdentLiteral IdentLiteral.Int; literal= "5"}
    ; {type_= Delimiter Delimiter.Semicolon; literal= ";"}
    ; {type_= Keyword Keyword.Let; literal= "let"}
    ; {type_= IdentLiteral IdentLiteral.Ident; literal= "ten"}
    ; {type_= Operator Operator.Assign; literal= "="}
    ; {type_= IdentLiteral IdentLiteral.Int; literal= "10"}
    ; {type_= Delimiter Delimiter.Semicolon; literal= ";"}
    ; {type_= Keyword Keyword.Let; literal= "let"}
    ; {type_= IdentLiteral IdentLiteral.Ident; literal= "add"}
    ; {type_= Operator Operator.Assign; literal= "="}
    ; {type_= Keyword Keyword.Function; literal= "fn"}
    ; {type_= Delimiter Delimiter.LParen; literal= "("}
    ; {type_= IdentLiteral IdentLiteral.Ident; literal= "x"}
    ; {type_= Delimiter Delimiter.Comma; literal= ","}
    ; {type_= IdentLiteral IdentLiteral.Ident; literal= "y"}
    ; {type_= Delimiter Delimiter.RParen; literal= ")"}
    ; {type_= Delimiter Delimiter.LBrace; literal= "{"}
    ; {type_= IdentLiteral IdentLiteral.Ident; literal= "x"}
    ; {type_= Operator Operator.Plus; literal= "+"}
    ; {type_= IdentLiteral IdentLiteral.Ident; literal= "y"}
    ; {type_= Delimiter Delimiter.Semicolon; literal= ";"}
    ; {type_= Delimiter Delimiter.RBrace; literal= "}"} (* <= here *)
    ; {type_= Delimiter Delimiter.Semicolon; literal= ";"}
    ; {type_= Keyword Keyword.Let; literal= "let"}
    ; {type_= IdentLiteral IdentLiteral.Ident; literal= "result"}
    ; {type_= Operator Operator.Assign; literal= "="}
    ; {type_= IdentLiteral IdentLiteral.Ident; literal= "add"}
    ; {type_= Delimiter Delimiter.LParen; literal= "("}
    ; {type_= IdentLiteral IdentLiteral.Ident; literal= "five"}
    ; {type_= Delimiter Delimiter.Comma; literal= ","}
    ; {type_= IdentLiteral IdentLiteral.Ident; literal= "ten"}
    ; {type_= Delimiter Delimiter.RParen; literal= ")"}
    ; {type_= Delimiter Delimiter.Semicolon; literal= ";"}
    ; {type_= Operator Operator.Bang; literal= "!"}
    ; {type_= Operator Operator.Minus; literal= "-"}
    ; {type_= Operator Operator.Slash; literal= "/"}
    ; {type_= Operator Operator.Asterisk; literal= "*"}
    ; {type_= IdentLiteral IdentLiteral.Int; literal= "5"}
    ; {type_= Delimiter Delimiter.Semicolon; literal= ";"}
    ; {type_= IdentLiteral IdentLiteral.Int; literal= "5"}
    ; {type_= Operator Operator.LessThan; literal= "<"}
    ; {type_= IdentLiteral IdentLiteral.Int; literal= "10"}
    ; {type_= Operator Operator.GreaterThan; literal= ">"}
    ; {type_= IdentLiteral IdentLiteral.Int; literal= "5"}
    ; {type_= Delimiter Delimiter.Semicolon; literal= ";"}
    ; {type_= Keyword Keyword.If; literal= "if"}
    ; {type_= Delimiter Delimiter.LParen; literal= "("}
    ; {type_= IdentLiteral IdentLiteral.Int; literal= "5"}
    ; {type_= Operator Operator.LessThan; literal= "<"}
    ; {type_= IdentLiteral IdentLiteral.Int; literal= "10"}
    ; {type_= Delimiter Delimiter.RParen; literal= ")"}
    ; {type_= Delimiter Delimiter.LBrace; literal= "{"}
    ; {type_= Keyword Keyword.Return; literal= "return"}
    ; {type_= Keyword Keyword.True; literal= "true"}
    ; {type_= Delimiter Delimiter.Semicolon; literal= ";"}
    ; {type_= Delimiter Delimiter.RBrace; literal= "}"}
    ; {type_= Keyword Keyword.Else; literal= "else"}
    ; {type_= Delimiter Delimiter.LBrace; literal= "{"}
    ; {type_= Keyword Keyword.Return; literal= "return"}
    ; {type_= Keyword Keyword.False; literal= "false"}
    ; {type_= Delimiter Delimiter.Semicolon; literal= ";"}
    ; {type_= Delimiter Delimiter.RBrace; literal= "}"}
    ; {type_= IdentLiteral IdentLiteral.Int; literal= "10"}
    ; {type_= Operator Operator.Equal; literal= "=="}
    ; {type_= IdentLiteral IdentLiteral.Int; literal= "10"}
    ; {type_= Delimiter Delimiter.Semicolon; literal= ";"}
    ; {type_= IdentLiteral IdentLiteral.Int; literal= "10"}
    ; {type_= Operator Operator.NotEqual; literal= "!="}
    ; {type_= IdentLiteral IdentLiteral.Int; literal= "9"}
    ; {type_= Delimiter Delimiter.Semicolon; literal= ";"}
    ; {type_= Meta Meta.EOF; literal= ""} ]


  let lexer = Lexer.of_string input

  let _ =
    List.foldi expected
      ~f:(fun n current_lexer expected_token ->
        Out_channel.print_endline @@ Int.to_string n ;
        let lexed_lexer, lexed_token = Lexer.next_token current_lexer in
        if not @@ Token.TokenType.equal lexed_token.type_ expected_token.type_ then
          Out_channel.printf "tests[%d] - tokentype wrong. expected=%s, got=%s\n" n
            (Token.TokenType.to_string expected_token.type_)
            (Token.TokenType.to_string lexed_token.type_) ;
        if not @@ String.equal lexed_token.literal expected_token.literal then
          Out_channel.printf "tests[%d] - literal wrong. expected=%s, got=%s\n" n
            expected_token.literal lexed_token.literal ;
        lexed_lexer )
      ~init:lexer


  let _ = "end"
end

module ASTTest = struct
  open AST

  let example = "let myVar = anotherVar;"

  let let_token : Token.t = {type_= Keyword Keyword.Let; literal= "let"}

  let myVar : Identifier.t =
    {token= {type_= IdentLiteral IdentLiteral.Ident; literal= "myVar"}; value= "myVar"}


  let anotherVar : Expression.t =
    Identifier
      {token= {type_= IdentLiteral IdentLiteral.Ident; literal= "anotherVar"}; value= "anotherVar"}


  let statement : Statement.t = Let {token= let_token; name= myVar; value= Some anotherVar}

  let program : Program.t = [statement]

  let _ = assert (String.equal example @@ Program.to_string program)

  let _ = "end"
end

module ParserTest1 = struct
  let input = {|let x = 5;
     let y = 10;
     let foobar = 838383;|}

  (* let input = "foobar;" *)

  let parser = input |> Lexer.of_string |> Parser.of_lexer

  let program = Parser.parse_program parser

  (** Test if a given let statement has an identifier of a given name. *)
  let test_let_statement ((stmt, name) : AST.Statement.t * String.t) : Unit.t =
    match stmt with
    | Let let_stmt ->
        assert (String.equal let_stmt.token.literal "let") ;
        (* The name can be accessed either via the AST node's identifier value or the underlying token's literal value. *)
        assert (String.equal let_stmt.name.value name) ;
        assert (String.equal let_stmt.name.token.literal name)
    | _ ->
        raise @@ Invalid_argument "Well that's not an expression statement."


  let _ = List.iter ~f:test_let_statement @@ List.zip_exn program ["x"; "y"; "foobar"]
  (* Fails for now, since parse_program's not fully cooked yet *)

  let _ = "end"
end

module ParserTest2 = struct
  open AST

  let _ = "end"
end
