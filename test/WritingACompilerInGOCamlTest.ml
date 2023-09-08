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
    ; {type_= Delimiter Delimiter.RBrace; literal= ")"}
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
    ; {type_= Delimiter Delimiter.RBrace; literal= ")"}
    ; {type_= Keyword Keyword.Else; literal= "else"}
    ; {type_= Delimiter Delimiter.LBrace; literal= "{"}
    ; {type_= Keyword Keyword.Return; literal= "return"}
    ; {type_= Keyword Keyword.False; literal= "false"}
    ; {type_= Delimiter Delimiter.Semicolon; literal= ";"}
    ; {type_= Delimiter Delimiter.RBrace; literal= ")"}
    ; {type_= IdentLiteral IdentLiteral.Int; literal= "10"}
    ; {type_= Operator Operator.Equal; literal= "=="}
    ; {type_= IdentLiteral IdentLiteral.Int; literal= "10"}
    ; {type_= Delimiter Delimiter.Semicolon; literal= ";"}
    ; {type_= IdentLiteral IdentLiteral.Int; literal= "10"}
    ; {type_= Operator Operator.NotEqual; literal= "!="}
    ; {type_= IdentLiteral IdentLiteral.Int; literal= "9"}
    ; {type_= Delimiter Delimiter.Semicolon; literal= ";"}
    ; {type_= Meta Meta.EOF; literal= ""} ]
end
