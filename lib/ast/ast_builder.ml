open Core
open Grace
open Ast_types
open Ast

module type Range = sig
  val v : Range.t
end

module Dummy_range : Range = struct
  let source : Source.t =
    `String { name = Some "Ast_builder.Dummy_range.source"; content = "" }
  ;;

  let v = Range.initial source
end

module type S = sig
  type 'a with_range_fn

  val type_var_name : (string -> type_variable_name) with_range_fn
  val type_name : (string -> type_name) with_range_fn
  val var_name : (string -> variable_name) with_range_fn
  val constr_name : (string -> constructor_name) with_range_fn
  val label_name : (string -> label_name) with_range_fn

  module Type : sig
    val var : (type_variable_name -> core_type) with_range_fn
    val arrow : (core_type -> core_type -> core_type) with_range_fn
    val tuple : (core_type list -> core_type) with_range_fn
    val constr : (core_type list -> type_name -> core_type) with_range_fn
    val scheme : (type_variable_name list -> core_type -> core_scheme) with_range_fn
  end

  module Pattern : sig
    val any : pattern with_range_fn
    val var : (variable_name -> pattern) with_range_fn
    val alias : (pattern -> as_:variable_name -> pattern) with_range_fn
    val const : (constant -> pattern) with_range_fn
    val tuple : (pattern list -> pattern) with_range_fn
    val constr : (constructor_name -> pattern option -> pattern) with_range_fn
    val annot : (pattern -> core_type -> pattern) with_range_fn
  end

  module Expression : sig
    val var : (variable_name -> expression) with_range_fn
    val const : (constant -> expression) with_range_fn
    val fun_ : (pattern -> expression -> expression) with_range_fn
    val app : (expression -> expression -> expression) with_range_fn

    val let_
      : (?rec_flag:rec_flag -> value_binding list -> in_:expression -> expression)
          with_range_fn

    val exists : (type_variable_name list -> expression -> expression) with_range_fn
    val annot : (expression -> core_type -> expression) with_range_fn
    val constr : (constructor_name -> expression option -> expression) with_range_fn
    val record : ((label_name * expression) list -> expression) with_range_fn
    val field : (expression -> label_name -> expression) with_range_fn
    val tuple : (expression list -> expression) with_range_fn
    val match_ : (expression -> with_:case list -> expression) with_range_fn

    val if_
      : (expression -> then_:expression -> else_:expression -> expression) with_range_fn

    val sequence : (expression -> expression -> expression) with_range_fn
    val case : (lhs:pattern -> rhs:expression -> case) with_range_fn

    (** Sugar *)
    val fun_many : (pattern list -> expression -> expression) with_range_fn

    val sequence_many : (expression list -> expression) with_range_fn
  end

  val value_binding : (pattern -> expression -> value_binding) with_range_fn

  module Structure : sig
    val value : (?rec_flag:rec_flag -> value_binding list -> structure_item) with_range_fn
    val primitive : (value_description -> structure_item) with_range_fn
    val type_ : (type_declaration list -> structure_item) with_range_fn

    val type_decl
      : (name:type_name
         -> params:type_variable_name list
         -> type_decl_kind
         -> type_declaration)
          with_range_fn

    val value_desc
      : (name:variable_name -> type_:core_scheme -> value_description) with_range_fn

    val label_decl : name:label_name -> arg:core_type -> label_declaration

    val constr_decl
      :  name:constructor_name
      -> arg:core_type option
      -> constructor_declaration
  end
end

module Default : S with type 'a with_range_fn := range:Range.t -> 'a = struct
  let type_var_name ~range name = Type_var_name (With_range.create ~range name)
  let var_name ~range name = Var_name (With_range.create ~range name)
  let constr_name ~range name = Constructor_name (With_range.create ~range name)
  let type_name ~range name = Type_name (With_range.create ~range name)
  let label_name ~range name = Label_name (With_range.create ~range name)

  module Type = struct
    let var ~range type_var_name = With_range.create ~range @@ Type_var type_var_name
    let arrow ~range type1 type2 = With_range.create ~range @@ Type_arrow (type1, type2)
    let tuple ~range types = With_range.create ~range @@ Type_tuple types

    let constr ~range arg_types constr_name =
      With_range.create ~range @@ Type_constr (arg_types, constr_name)
    ;;

    let scheme ~range type_var_names type_ =
      With_range.create ~range @@ (type_var_names, type_)
    ;;
  end

  module Pattern = struct
    let any ~range = With_range.create ~range Pat_any
    let var ~range var_name = With_range.create ~range @@ Pat_var var_name
    let alias ~range pat ~as_ = With_range.create ~range @@ Pat_alias (pat, as_)
    let const ~range const = With_range.create ~range @@ Pat_const const
    let tuple ~range pats = With_range.create ~range @@ Pat_tuple pats

    let constr ~range constr_name arg_pat =
      With_range.create ~range @@ Pat_constr (constr_name, arg_pat)
    ;;

    let annot ~range pat type_ = With_range.create ~range @@ Pat_annot (pat, type_)
  end

  module Expression = struct
    let var ~range var_name = With_range.create ~range @@ Exp_var var_name
    let const ~range const = With_range.create ~range @@ Exp_const const
    let fun_ ~range pat exp = With_range.create ~range @@ Exp_fun (pat, exp)
    let app ~range exp1 exp2 = With_range.create ~range @@ Exp_app (exp1, exp2)

    let let_ ~range ?(rec_flag = Nonrecursive) value_bindings ~in_ =
      With_range.create ~range @@ Exp_let (rec_flag, value_bindings, in_)
    ;;

    let exists ~range type_var_names exp =
      With_range.create ~range @@ Exp_exists (type_var_names, exp)
    ;;

    let annot ~range exp type_ = With_range.create ~range @@ Exp_annot (exp, type_)

    let constr ~range constr_name arg_exp =
      With_range.create ~range @@ Exp_constr (constr_name, arg_exp)
    ;;

    let record ~range label_exps = With_range.create ~range @@ Exp_record label_exps

    let field ~range exp label_name =
      With_range.create ~range @@ Exp_field (exp, label_name)
    ;;

    let tuple ~range exps = With_range.create ~range @@ Exp_tuple exps
    let match_ ~range exp ~with_ = With_range.create ~range @@ Exp_match (exp, with_)

    let if_ ~range exp ~then_ ~else_ =
      With_range.create ~range @@ Exp_if_then_else (exp, then_, else_)
    ;;

    let sequence ~range exp1 exp2 = With_range.create ~range @@ Exp_sequence (exp1, exp2)

    let case ~range ~lhs ~rhs =
      With_range.create ~range @@ { case_lhs = lhs; case_rhs = rhs }
    ;;

    (* TODO: Fix ranges on sugar *)

    let rec fun_many ~range pats exp =
      match pats with
      | [] -> assert false
      | [ pat ] -> fun_ ~range pat exp
      | pat :: pats -> fun_ ~range pat (fun_many ~range pats exp)
    ;;

    let rec sequence_many ~range exps =
      match exps with
      | [] -> assert false
      | [ exp ] -> exp
      | [ exp1; exp2 ] -> sequence ~range exp1 exp2
      | exp :: exps -> sequence ~range exp (sequence_many ~range exps)
    ;;
  end

  let value_binding ~range pat exp =
    With_range.create ~range { value_binding_exp = exp; value_binding_pat = pat }
  ;;

  module Structure = struct
    let value ~range ?(rec_flag = Nonrecursive) value_bindings =
      With_range.create ~range @@ Str_value (rec_flag, value_bindings)
    ;;

    let primitive ~range value_desc = With_range.create ~range @@ Str_primitive value_desc
    let type_ ~range type_decls = With_range.create ~range @@ Str_type type_decls

    let type_decl ~range ~name ~params kind =
      With_range.create
        ~range
        { type_decl_name = name; type_decl_params = params; type_decl_kind = kind }
    ;;

    let value_desc ~range ~name ~type_ =
      With_range.create ~range { value_name = name; value_type = type_ }
    ;;

    let label_decl ~name ~arg = { label_name = name; label_arg = arg }
    let constr_decl ~name ~arg = { constructor_name = name; constructor_arg = arg }
  end
end

module Make (R : Range) : S with type 'a with_range_fn := 'a = struct
  open Default

  let type_var_name = type_var_name ~range:R.v
  let var_name = var_name ~range:R.v
  let constr_name = constr_name ~range:R.v
  let type_name = type_name ~range:R.v
  let label_name = label_name ~range:R.v

  module Type = struct
    let var = Type.var ~range:R.v
    let arrow = Type.arrow ~range:R.v
    let tuple = Type.tuple ~range:R.v
    let constr = Type.constr ~range:R.v
    let scheme = Type.scheme ~range:R.v
  end

  module Pattern = struct
    let any = Pattern.any ~range:R.v
    let var = Pattern.var ~range:R.v
    let alias = Pattern.alias ~range:R.v
    let const = Pattern.const ~range:R.v
    let tuple = Pattern.tuple ~range:R.v
    let constr = Pattern.constr ~range:R.v
    let annot = Pattern.annot ~range:R.v
  end

  module Expression = struct
    let var = Expression.var ~range:R.v
    let const = Expression.const ~range:R.v
    let fun_ = Expression.fun_ ~range:R.v
    let app = Expression.app ~range:R.v
    let let_ = Expression.let_ ~range:R.v
    let exists = Expression.exists ~range:R.v
    let annot = Expression.annot ~range:R.v
    let constr = Expression.constr ~range:R.v
    let record = Expression.record ~range:R.v
    let field = Expression.field ~range:R.v
    let tuple = Expression.tuple ~range:R.v
    let match_ = Expression.match_ ~range:R.v
    let if_ = Expression.if_ ~range:R.v
    let sequence = Expression.sequence ~range:R.v
    let case = Expression.case ~range:R.v
    let fun_many = Expression.fun_many ~range:R.v
    let sequence_many = Expression.sequence_many ~range:R.v
  end

  let value_binding = value_binding ~range:R.v

  module Structure = struct
    let value = Structure.value ~range:R.v
    let primitive = Structure.primitive ~range:R.v
    let type_ = Structure.type_ ~range:R.v
    let type_decl = Structure.type_decl ~range:R.v
    let value_desc = Structure.value_desc ~range:R.v
    let label_decl = Structure.label_decl
    let constr_decl = Structure.constr_decl
  end
end

module Dummy = Make (Dummy_range)
