type syntax_t =
  | VariableSyntax of string
  | NotSyntax of syntax_t
  | AndSyntax of syntax_t * syntax_t
  | OrSyntax of syntax_t * syntax_t
  | XorSyntax of syntax_t * syntax_t
  | ImpliesSyntax of syntax_t * syntax_t
