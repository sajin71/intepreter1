open Expr
open Format

let print_value result = match result with
    | VInt(x) -> print_int x
    | VBool(x) -> print_bool x

let _ = 
  try 
      let lexbuf = if Array.length Sys.argv > 1 then Lexing.from_channel (open_in Sys.argv.(1)) else Lexing.from_channel stdin in 
        let rec loop () = 
         let result = 
           ExampleParser.main ExampleLexer.token lexbuf in 
        print_value (eval [] result); print_newline (); flush stdout;
        loop () in 
      loop ()
  with End_of_file -> exit 0 
