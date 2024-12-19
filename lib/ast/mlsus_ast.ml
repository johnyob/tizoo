module Ast_types = Ast_types
module Ast = Ast
module Range = Grace.Range

type 'a with_range = 'a Ast_types.with_range =
  { it : 'a
  ; range : Range.t
  }
[@@deriving sexp_of]
