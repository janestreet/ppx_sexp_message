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
  | Const_int       _ -> f "int"
  | Const_char      _ -> f "char"
  | Const_string    _ -> f "string"
  | Const_float     _ -> f "float"
  | Const_int32     _ -> f "int32"
  | Const_int64     _ -> f "int64"
  | Const_nativeint _ -> f "nativeint"
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
  | Pexp_constant (Const_string ("", _)) ->
    None
  | Pexp_constant const ->
    Some (sexp_of_constant ~loc const)
  | Pexp_constraint (expr, ctyp) ->
    Some (sexp_of_constraint ~loc expr ctyp)
  | _ -> Some [%expr Sexplib.Conv.sexp_of_string [%e e]]
;;

type arg_label =
  | Nolabel
  | Labelled of string
  | Optional

(* Will help with the switch to 4.03 *)
let arg_label_of_string = function
  | "" -> Nolabel
  | s when s.[0] = '?' -> Optional
  | s -> Labelled s
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
  | Optional, _ ->
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
      (Nolabel, f) :: List.map args ~f:(fun (label, e) -> arg_label_of_string label, e)
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
