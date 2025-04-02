open Token
open Token.TokenType
module F = Format

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

let lex_completely (input : String.t) : Token.t list =
  let rec inner (current_lexer : Lexer.t) (current_input_list : Token.t list) : Token.t list =
    if Int.( > ) current_lexer.read_position @@ String.length lexer.input then
      List.rev ({type_= Meta Meta.EOF; literal= ""} :: current_input_list)
    else
      let new_lexer, lexed_token = Lexer.next_token current_lexer in
      inner new_lexer (lexed_token :: current_input_list)
  in
  inner (Lexer.of_string input) []


let pp_token_list fmt (token_list : Token.t list) =
  F.fprintf fmt "[\n" ;
  List.iter ~f:(fun token -> F.fprintf fmt "\t%a\n" Token.pp token) token_list ;
  F.fprintf fmt "]"


let token_list_equal (token_list_1 : Token.t list) (token_list_2 : Token.t list) =
  List.equal Token.equal token_list_1 token_list_2


let token_list_testable = Alcotest.testable pp_token_list token_list_equal

let test_token_list () =
  let actual = lex_completely input in
  Alcotest.check token_list_testable "Lexer test on a sample program" expected actual


let test_suite = [("Testing Lexer", `Quick, test_token_list)]

let () = Alcotest.run "Lexer test" [("Lexer test", test_suite)]
