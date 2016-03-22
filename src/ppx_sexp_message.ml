open StdLabels
open Ppx_core.Std
open Asttypes
open Parsetree
open Ast_builder.Default

module List = struct
  include List
  let filter_map =
    let rec aux acc f = function
      | [] -> rev acc
      | h :: t ->
        match f h with
        | None -> aux acc f t
        | Some h2 -> aux (h2 :: acc) f t
    in
    fun l ~f -> aux [] f l

  let of_option = function
    | None   -> []
    | Some x -> [x]
end

[@@@metaloc loc];;

let sexp_atom ~loc x = [%expr Sexplib.Sexp.Atom [%e x]]
let sexp_list ~loc x = [%expr Sexplib.Sexp.List [%e x]]

let sexp_inline ~loc l =
  match l with
  | [x] -> x
  | _   -> sexp_list ~loc (elist ~loc l)
;;

let sexp_of_constraint ~loc expr ctyp =
  let sexp_of = Ppx_sexp_conv_expander.Sexp_of.core_type ctyp in
  eapply ~loc sexp_of [expr]
;;

let sexp_of_constant ~loc const =
  let f typ =
    eapply ~loc (evar ~loc ("Sexplib.Conv.sexp_of_" ^ typ)) [pexp_constant ~loc const]
  in
  match const with
  | Pconst_integer       _ -> f "int"
  | Pconst_char      _ -> f "char"
  | Pconst_string    _ -> f "string"
  | Pconst_float     _ -> f "float"
;;

let rewrite_here e =
  match e.pexp_desc with
  | Pexp_extension ({ txt = "here"; _ }, PStr []) ->
    Ppx_here_expander.lift_position_as_string ~loc:e.pexp_loc
  | _ -> e
;;

let sexp_of_expr e =
  let e = rewrite_here e in
  let loc = e.pexp_loc in
  match e.pexp_desc with
  | Pexp_constant (Pconst_string ("", _)) ->
    None
  | Pexp_constant const ->
    Some (sexp_of_constant ~loc const)
  | Pexp_constraint (expr, ctyp) ->
    Some (sexp_of_constraint ~loc expr ctyp)
  | _ -> Some [%expr Sexplib.Conv.sexp_of_string [%e e]]
;;

let sexp_of_labelled_expr (label, e) =
  let loc = e.pexp_loc in
  match label, e.pexp_desc with
  | Nolabel, Pexp_constraint (expr, _) ->
    let expr_str = Pprintast.string_of_expression expr in
    Some (sexp_inline ~loc (sexp_atom ~loc (estring ~loc expr_str)
                            :: List.of_option (sexp_of_expr e)))
  | Nolabel, _ ->
    sexp_of_expr e
  | Labelled "_", _ ->
    sexp_of_expr e
  | Labelled label, _ ->
    Some (sexp_inline ~loc (sexp_atom ~loc (estring ~loc label)
                            :: List.of_option (sexp_of_expr e)))
  | Optional _, _ ->
    (* Could be used to encode sexp_option if that's ever needed. *)
    Location.raise_errorf ~loc
      "ppx_sexp_value: optional argument not allowed here"
;;

let sexp_of_labelled_exprs ~loc labels_and_exprs =
  sexp_inline ~loc (List.filter_map labels_and_exprs ~f:sexp_of_labelled_expr)
;;

let expand ~loc:_ ~path:_ e =
  let loc = e.pexp_loc in
  let labelled_exprs =
    match e.pexp_desc with
    | Pexp_apply (f, args) ->
      (Nolabel, f) :: args
    | _ ->
      (Nolabel, e) :: []
  in
  sexp_of_labelled_exprs ~loc labelled_exprs
;;

let message =
  Extension.V2.declare "message" Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    expand
;;

let () =
  Ppx_driver.register_transformation "sexp_message"
    ~extensions:[ message ]
;;
