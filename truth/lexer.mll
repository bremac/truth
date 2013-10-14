(* *)

{
  open Parser
  exception Error of string
}

let whitespace = [' ' '\t' '\n']
let variable = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9']*

rule token = parse
  | whitespace { token lexbuf }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | "not" { NOT }
  | "and" { AND }
  | "or" { OR }
  | "xor" { XOR }
  | "implies" { IMPLIES }
  | eof { EOF }
  | variable as s { VARIABLE s }
  | _
      { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }
      
