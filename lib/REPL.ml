let prompt = ">> "

let rec start () : unit =
  let open Token in
  let open TokenType in
  let rec print_next_token_until_EOF (lexer : Lexer.t) : unit =
    let lexed_lexer, lexed_token = Lexer.next_token lexer in
    match lexed_token with
    | {type_= Meta Meta.EOF; _} ->
        ()
    | token ->
        Out_channel.print_endline @@ F.asprintf "%a" Token.pp token ;
        print_next_token_until_EOF lexed_lexer
  in
  Out_channel.print_string prompt ;
  Out_channel.flush Out_channel.stdout ;
  try
    let scanned = Stdlib.read_line () in
    let lexer = Lexer.of_string scanned in
    print_next_token_until_EOF lexer ;
    start ()
  with Scanf.Scan_failure _ | Failure _ | End_of_file -> ()
