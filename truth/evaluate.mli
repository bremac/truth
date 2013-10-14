module Binding : Map.S with type key = string

module Variable : Set.S with type elt = string

val extract_variables : Expression.expression_t -> Variable.t

val map_bindings : Binding.key list -> (bool Binding.t -> 'a) -> 'a list

val evaluate : Expression.expression_t -> bool Binding.t -> bool

val compute_table : Expression.expression_t -> (bool Binding.t * bool) list
