{
    open ExampleParser
}

let digit = ['0' - '9']
let lower = ['a' - 'z']
let upper = ['A' - 'Z']
rule token = parse
  | [' ' '\t']* { token lexbuf }
  | '\n'        { EOL }
  | digit+ as n { INT (int_of_string n) }
  | "true"      { TRUE (bool_of_string "true") }
  | "false"     { FALSE (bool_of_string "false") }
  | '+'         { PLUS }
  | '-'         { MINUS }
  | '*'         { TIMES }
  | '/'         { DIV }
  | '('         { LPAR }
  | ')'         { RPAR }
  | '='         { EQ }
  | '<'         { GT }
  | '>'         { LT }
  | "&&"        { AND }
  | "||"        { OR }
  | "not"       { NOT }
  | "if"        { IF }
  | "then"      { THEN }
  | "else"      { ELSE }
  | "let"		{ LET }
  | "in"		{ IN }
  | "(*"        { comment lexbuf; token lexbuf }
  | lower (digit|lower|upper)* as n { ID( n )}
  | eof         { raise End_of_file }
  | _           { failwith "Unrecognized Character" }

  and comment = parse
  | "*)"    { () }
  | "(*"    { comment lexbuf; comment lexbuf }
  | eof     {  print_string "Lex error: unterminated comment\n";failwith "unterminated comment" }
  | _       { comment lexbuf }
