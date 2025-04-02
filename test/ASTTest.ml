open AST
open Token.TokenType

module LetStatementTest = struct
  let example = "let myVar = anotherVar;"

  let let_token : Token.t = {type_= Keyword Keyword.Let; literal= "let"}

  let myVar : Identifier.t =
    {token= {type_= IdentLiteral IdentLiteral.Ident; literal= "myVar"}; value= "myVar"}


  let anotherVar : Expression.t =
    Identifier
      {token= {type_= IdentLiteral IdentLiteral.Ident; literal= "anotherVar"}; value= "anotherVar"}


  let statement : Statement.t = Let {token= let_token; name= myVar; value= Some anotherVar}

  let program : Program.t = [statement]

  let test_let_statement = Alcotest.check Alcotest.string example (Program.to_string program)

  let tests = [("Let Statement", `Quick, test_let_statement)]
end
