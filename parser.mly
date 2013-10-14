%{
  open Syntax
%}

%token <string> VARIABLE
%token NOT
%token AND OR XOR IMPLIES
%token LPAREN RPAREN
%token EOF

%start <Syntax.syntax_t> main

%%

main:
| e = expr EOF { e }

atom:
| v = VARIABLE { VariableSyntax v }
| NOT a = atom { NotSyntax a }
| LPAREN e = expr RPAREN { e }

expr:
| a = atom IMPLIES e = expr { ImpliesSyntax (a, e) }
| a = atom XOR e = expr { XorSyntax (a, e) }
| a = atom AND e = expr { AndSyntax (a, e) }
| a = atom OR e = expr { OrSyntax (a, e) }
| a = atom { a }
