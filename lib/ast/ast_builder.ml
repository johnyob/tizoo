open Ast_types
open Ast

let type_var_name name = Type_var_name.create name
let var_name name = Var_name.create name
let constr_name name = Constructor_name.create name
let type_name name = Type_name.create name
let label_name name = Label_name.create name

module Type = struct
  let var type_var_name = Type_var type_var_name
  let arrow type1 type2 = Type_arrow (type1, type2)
  let tuple types = Type_tuple types
  let constr arg_types constr_name = Type_constr (arg_types, constr_name)

  let scheme ?(quantifiers = []) body =
    { scheme_quantifiers = quantifiers; scheme_body = body }
  ;;
end

module Pattern = struct
  let any = Pat_any
  let var var_name = Pat_var var_name
  let alias pat ~as_ = Pat_alias (pat, as_)
  let const const = Pat_const const
  let tuple pats = Pat_tuple pats
  let constr constr_name arg_pat = Pat_constr (constr_name, arg_pat)
  let annot pat type_ = Pat_annot (pat, type_)
end

module Expression = struct
  let var var_name = Exp_var var_name
  let const const = Exp_const const
  let fun_ pat exp = Exp_fun (pat, exp)
  let app exp1 exp2 = Exp_app (exp1, exp2)
  let let_ value_binding ~in_ = Exp_let (value_binding, in_)
  let exists type_var_names exp = Exp_exists (type_var_names, exp)
  let annot exp type_ = Exp_annot (exp, type_)
  let constr constr_name arg_exp = Exp_constr (constr_name, arg_exp)
  let tuple exps = Exp_tuple exps
  let match_ exp ~with_ = Exp_match (exp, with_)
  let if_ exp ~then_ ~else_ = Exp_if_then_else (exp, then_, else_)
  let sequence exp1 exp2 = Exp_sequence (exp1, exp2)
  let case ~lhs ~rhs = { case_lhs = lhs; case_rhs = rhs }

  let rec fun_many pats exp =
    match pats with
    | [] -> assert false
    | [ pat ] -> fun_ pat exp
    | pat :: pats -> fun_ pat (fun_many pats exp)
  ;;

  let rec sequence_many exps =
    match exps with
    | [] -> assert false
    | [ exp ] -> exp
    | [ exp1; exp2 ] -> sequence exp1 exp2
    | exp :: exps -> sequence exp (sequence_many exps)
  ;;
end

let value_binding var exp = { value_binding_exp = exp; value_binding_var = var }

module Structure = struct
  let value value_binding = Str_value value_binding
  let primitive value_desc = Str_primitive value_desc
  let type_ type_decls = Str_type type_decls

  let type_decl ~name ~params kind =
    { type_decl_name = name; type_decl_params = params; type_decl_kind = kind }
  ;;

  let value_desc ~name ~type_ = { value_name = name; value_type = type_ }
  let constr_decl ~name ~arg = { constructor_name = name; constructor_arg = arg }
end
