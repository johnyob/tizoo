%{
open Mlsus_ast 
open Ast_types
open Ast_builder

(* A set of predefined operator names *)
module Predef_names = struct 
  let or_ = var_name "( || )"
  let and_ = var_name "( && )"

  let equal = var_name "( = )"
  let not_equal = var_name "( <> )"
  let less_than = var_name "( < )"
  let greater_than = var_name "( > )"
  let less_than_equal = var_name "( <= )"
  let greater_than_equal = var_name "( >= )"
  
  let add = var_name "( + )"
  let sub = var_name "( - )"
  let mul = var_name "( * )"
  let div = var_name "( / )" 
  
  let neg = var_name "unary( - )"
end 

let binary_op ~op ~exp1 ~exp2 =
  Expression.(app (app (var op) exp1) exp2)

let unary_op ~op ~exp = 
  Expression.(app (var op) exp)
%}

%nonassoc prec_below_SEMI
%nonassoc ";"

%left "||"
%left "&&"
%left "=" "<>" "<" "<=" ">" ">="
%left "+" "-"
%left "*" "/" 
%nonassoc prec_unary_op

%start  parse_core_type parse_expression parse_structure
%type <Ast.structure> parse_structure
%type <Ast.expression> parse_expression
%type <Ast.core_type> parse_core_type

%%

(** {1 Start Symbols} *)

parse_structure:
  str = structure; EOF 
    { str }

parse_expression:
  exp = expression; EOF 
    { exp }

parse_core_type:
  type_ = core_type; EOF 
    { type_ }

(** {2 Standard Library Extensions} 

    This section contains definitions of several generic parameterized rules that 
    are useful for parsing mlsus. *)

(** [seperated_nontrivial_list(sep, X)] parases a list containing at least two [X]s 
    separated by [sep]. *)
separated_nontrivial_list(sep, X):
    x1 = X
    ; sep
    ; x2 = X
      { [ x1; x2 ] }
  | x = X
    ; sep
    ; xs = separated_nontrivial_list(sep, X)
      { x :: xs }

(** [preceded_or_separated_nonempty_list(sep, X)] parses a non-emppty list of [X]s separated 
    by [sep], optionally preceded by [sep]. *)
%inline preceded_or_separated_nonempty_list(sep, X):
  ioption(sep); xs = separated_nonempty_list(sep, X)
    { xs }


(** {3 Core Types} 

    This section contains the rules for parsing the grammar of core types:
    {v
      tau ::=   
        | 'a                (* variables *) 
        | tau -> tau        (* functions *)
        | tau * ... * tau   (* tuples *)
        | overline(tau) T   (* type constructors *)
    v} 
    
    The grammar is {e stratified} to handle the precedence issues that arise. *)

type_var_name:
  "'"; id = "<ident>"
    { type_var_name id }

type_name:
  id = "<ident>"
    { type_name id }

%inline core_type:
  type_ = arrow_type
    { type_ }

arrow_type:
    type_ = tuple_type
      { type_ }   
  | type1 = tuple_type
    ; "->"
    ; type2 = core_type
      { Type.arrow type1 type2 }

tuple_type:
    type_ = atom_type
      { type_ }
  | types = separated_nontrivial_list("*", atom_type)
      { Type.tuple types }

atom_type:
    "("
    ; type_ = core_type
    ; ")"
      { type_ }
  | type_var_name = type_var_name
      { Type.var type_var_name }
  | arg_types = type_argument_list
    ; type_name = type_name
      { Type.constr arg_types type_name }

%inline type_argument_list:
    (* empty *)   
      { [] }
  | type_ = atom_type 
      { [ type_ ] }
  | "("
    ; types = separated_nontrivial_list(",", core_type)
    ; ")"
      { types }


(** {4 Expressions and Patterns} 

    This section contains the rules for parsing the grammar of expressions
    {v
      e ::= 
        | x                                 (* variable *)
        | const                             (* constants *)
        | uop e                             (* unary operator *)
        | e bop e                           (* binary operator *)
        | if e then e else e                (* if-then-else *)
        | fun overline(p) -> e              (* functions *)
        | e e                               (* function application *)
        | exists (type overline('a)) -> e   (* type exists binder *)
        | match e with ( overline(c) )      (* match *)
        | let vb in e                       (* let *)
        | { l1 = e1; ...; ln = en }         (* record *)
        | (e1, ..., en)                     (* tuples *)
        | K e?                              (* constructor *)
        | e.l                               (* field *)
        | e; e                              (* sequence *)
        | (e : tau)                         (* type annotation *)
    v} 

    Expressions also include cases, patterns and value bindings, given by the following 
    grammars:
    {v
      p ::= 
        | x 
        | _
        | p as x 
        | K p?
        | { l1 = p1; ...; ln = pn }
        | (p1, ..., pn)
        | (p : tau)

      c ::= p -> e 

      vb ::= p = e
    v}
*)

%inline var_name:
  id = "<ident>"
    { var_name id }

%inline constr_name:
  uid = "<upper_ident>"
    { constr_name uid }

%inline label_name:
  id = "<ident>"
    { label_name id }

constant:
    int = "<int>"
      { Const_int int }
  | float = "<float>"
      { Const_float float }
  | "true"
      { Const_bool true }
  | "false"
      { Const_bool false }
  | string = "<string>"
      { Const_string string }
  | char = "<char>"
      { Const_char char }
  | "()"
      { Const_unit }

seq_expression:
    exp = expression %prec prec_below_SEMI     
      { exp }
  | exp1 = expression
    ; ";"
    ; exp2 = seq_expression
      { Expression.sequence exp1 exp2 }

expression:
    exp = app_expression                                                                   
      { exp }
  | op = unary_op
    ; exp = expression %prec prec_unary_op
      { unary_op ~op ~exp }
  | exp1 = expression
    ; op = bin_op
    ; exp2 = expression
      { binary_op ~op ~exp1 ~exp2 }
  | "if"
    ; cond = expression
    ; "then"
    ; then_ = seq_expression
    ; "else"
    ; else_ = seq_expression
      { Expression.if_ cond ~then_ ~else_ }
  | "fun"
    ; pats = nonempty_list(atom_pattern)
    ; "->"
    ; exp = seq_expression 
      { Expression.fun_many pats exp }
  | "exists"
    ; "("
    ; "type"
    ; type_var_names = nonempty_list(type_var_name)
    ; ")"
    ; "->"
    ; exp = seq_expression
      { Expression.exists type_var_names exp }
  | "match" 
    ; exp = expression 
    ; "with" 
    ; cases = cases
      { Expression.match_ exp ~with_:cases }
  | "let"
    ; value_binding = value_binding
    ; "in"
    ; exp = seq_expression
      { Expression.let_ value_binding ~in_:exp }

app_expression:
    exp = atom_expression
      { exp }
  | constr_name = constr_name 
    ; arg_exp = atom_expression
      { Expression.constr constr_name (Some arg_exp) }
  | exp1 = app_expression
    ; exp2 = atom_expression
      { Expression.app exp1 exp2 }

value_binding:
  pat = pattern 
  ; "="
  ; exp = seq_expression
    { value_binding pat exp }

cases:
  "("
  ; cases = preceded_or_separated_nonempty_list("|", case)
  ; ")"
    { cases }

case:
  pat = pattern
  ; "->"
  ; exp = seq_expression
      { Expression.case ~lhs:pat ~rhs:exp }


atom_expression:
    const = constant 
      { Expression.const const }
  | var_name = var_name
      { Expression.var var_name }
  | exp = atom_expression
    ; "."
    ; label_name = label_name
      { Expression.field exp label_name }
  | constr_name = constr_name
      { Expression.constr constr_name None }
  | "("
    ; exps = separated_nontrivial_list(",", seq_expression)
    ; ")"
      { Expression.tuple exps }
  | label_exps = record(expression)
      { Expression.record label_exps }
  | "("
    ; exp = seq_expression
    ; ":"
    ; type_ = core_type
    ; ")"
      { Expression.annot exp type_ }
  | "("
    ; exp = seq_expression
    ; ")"
      { exp }

record(X):
  "{"
  ; label_xs = separated_nonempty_list(";", record_assignment(X))
  ; "}"
    { label_xs }

record_assignment(X):
  label_name = label_name
  ; "="
  ; x = X
    { (label_name, x) }

%inline unary_op:
  "-" 
    { Predef_names.neg  }

%inline bin_op:
    "+"
      { Predef_names.add  }
  | "-"
      { Predef_names.sub  }
  | "/"
      { Predef_names.div  }
  | "*"
      { Predef_names.mul  }
  | ">"
      { Predef_names.greater_than  }  
  | "<"
      { Predef_names.less_than  }
  | ">="
      { Predef_names.greater_than_equal  }
  | "<="
      { Predef_names.less_than_equal  }
  | "="
      { Predef_names.equal  }
  | "<>"
      { Predef_names.not_equal  }
  | "&&"
      { Predef_names.and_  }
  | "||"
      { Predef_names.or_  }

pattern:
    pat = construct_pattern
      { pat }
  | pat = pattern
    ; "as"
    ; var_name = var_name
      { Pattern.alias pat ~as_:var_name }

construct_pattern:
    pat = atom_pattern
      { pat }
  | constr_name = constr_name
    ; arg_pat = pattern
      { Pattern.constr constr_name (Some arg_pat) }
 

atom_pattern:
    const = constant
      { Pattern.const const }
  | "_"     
      { Pattern.any }
  | var_name = var_name            
      { Pattern.var var_name }
  | constr_name = constr_name
      { Pattern.constr constr_name None }
  | label_pats = record(pattern)
      { Pattern.record label_pats }
  | "("
    ; pats = separated_nontrivial_list(",", pattern)
    ; ")"
      { Pattern.tuple pats }
  | "("
    ; pat = pattern
    ; ":"
    ; type_ = core_type
    ; ")"
      { Pattern.annot pat type_ }
  | "("
    ; pat = pattern
    ; ")"
      { pat }  


(** {5 Structures} 

    Structures are simply a list of type definitons, primitive declarations and 
    value bindings. 
*)

core_scheme:
    type_var_names = nonempty_list(type_var_name)
    ; "."
    ; type_ = core_type
      { Type.scheme ~quantifiers:type_var_names type_ }
  | type_ = core_type
      { Type.scheme type_ }

type_declarations:
  "type"
  ; decls = separated_nonempty_list("and", type_declaration)
    { decls }

type_declaration: 
  params = type_param_list 
  ; name = type_name 
  ; kind = type_decl_kind 
      { Structure.type_decl ~name ~params kind }

type_decl_kind:
    (* empty *)
      { Type_decl_abstract }
  | "="
    ; constr_decls = preceded_or_separated_nonempty_list("|", constructor_declaration)
      { Type_decl_variant constr_decls }
  | "="
    ; "{"
    ; label_decls = separated_nonempty_list(";", label_declaration)
    ; "}"
      { Type_decl_record label_decls }
      
label_declaration:
  name = label_name
  ; ":"
  ; arg = core_type
    { Structure.label_decl ~name ~arg }

constructor_declaration:
  name = constr_name
  ; arg = option(constructor_argument)
    { Structure.constr_decl ~name ~arg }

%inline constructor_argument:
  "of"
  ; type_ = core_type
    { type_ }

%inline type_param_list:
    (* empty *)   
      { [] }
  | type_var_name = type_var_name 
      { [ type_var_name ] }
  | "("
    ; type_var_names = separated_nontrivial_list(",", type_var_name)
    ; ")"
      { type_var_names }

structure_item:
    "let"
    ; value_binding = value_binding
      { Structure.value value_binding }
  | "external"
    ; name = var_name
    ; ":"
    ; scheme = core_scheme
      { Structure.(primitive (value_desc ~name ~type_:scheme)) }
  | type_decls = type_declarations
      { Structure.type_ type_decls }
  
terminated_structure_item:
  str_item = structure_item
  ; ";;"
    { str_item }

structure:
  structure = nonempty_list(terminated_structure_item)
    { structure }

