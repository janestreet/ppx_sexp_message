open Base
open Ppxlib
open Ast_builder.Default

let omit_nil_attr =
  Attribute.declare
    "sexp_message.sexp.omit_nil"
    Attribute.Context.core_type
    Ast_pattern.(pstr nil)
    ()
;;

let option_attr =
  Attribute.declare
    "sexp_message.sexp.option"
    Attribute.Context.core_type
    Ast_pattern.(pstr nil)
    ()
;;

let sexp_atom ~loc x = [%expr Ppx_sexp_conv_lib.Sexp.Atom [%e x]]
let sexp_list ~loc x = [%expr Ppx_sexp_conv_lib.Sexp.List [%e x]]

let sexp_inline ~loc l =
  match l with
  | [ x ] -> x
  | _ -> sexp_list ~loc (elist ~loc l)
;;

(* Same as Ppx_sexp_value.omittable_sexp *)
type omittable_sexp =
  | Present of expression
  | Optional of Location.t * expression * (expression -> expression)
  | Omit_nil of Location.t * expression * (expression -> expression)
  | Absent

let present_or_omit_nil ~loc ~omit_nil expr =
  if omit_nil then Omit_nil (loc, expr, Fn.id) else Present expr
;;

let wrap_sexp_if_present omittable_sexp ~f =
  match omittable_sexp with
  | Present e -> Present (f e)
  | Optional (loc, e, k) -> Optional (loc, e, fun e -> f (k e))
  | Omit_nil (loc, e, k) -> Omit_nil (loc, e, fun e -> f (k e))
  | Absent -> Absent
;;

(* [rename] returns a fresh name for an expression.

   You can later retrieve all names (in order) by calling [names], and the corresponding
   named expressions (in the same order) by calling [original_expressions].

   This module is used to construct a function that doesn't close over as many free
   variables. The strategy is: call [rename] on every expression you want to avoid closing
   over, and use the returned name within the body of the function. Later, use [names] to
   determine the parameters to the function, and [original_expressions] to determine the
   arguments to pass at the call site.

   See [wrap_in_cold_function] for why we only rename idents, not all expressions.
*)
module Renaming_env : sig
  type t

  val create : unit -> t
  val rename : t -> expression -> name_prefix:string option -> expression
  val names : t -> pattern list
  val original_expressions : t -> expression list
end = struct
  module Entry = struct
    type t =
      { name : string
      ; name_loc : Location.t
      ; original_expression : expression
      }

    let name_as_pattern t = pvar t.name ~loc:t.name_loc
    let original_expression t = t.original_expression
  end

  type t = Entry.t Queue.t

  let create () = Queue.create ()

  let rename t original_expression ~name_prefix =
    let name_loc = { original_expression.pexp_loc with loc_ghost = true } in
    let name = gen_symbol ?prefix:name_prefix () in
    let entry : Entry.t = { original_expression; name; name_loc } in
    Queue.enqueue t entry;
    evar ~loc:name_loc entry.name
  ;;

  let names t = Queue.to_list t |> List.map ~f:Entry.name_as_pattern
  let original_expressions t = Queue.to_list t |> List.map ~f:Entry.original_expression
end

let rename_if_ident_or_field_access env expression =
  let longident_to_human_friendly_string longident =
    (* Make a best-effort attempt to generate a name that roughly
       corresponds to the original name, to aid human readers of
       the ppx-generated code.
    *)
    match longident with
    | Lident ident -> Some ident
    | Ldot (_path, after_dot) -> Some after_dot
    | Lapply _ -> None
  in
  match expression.pexp_desc with
  | Pexp_ident ident ->
    let name_prefix = longident_to_human_friendly_string ident.txt in
    Renaming_env.rename env expression ~name_prefix
  | Pexp_field (record, field) ->
    let rec flatten_nested_field_accesses expression field_names =
      match expression.pexp_desc with
      | Pexp_field (record, field) ->
        flatten_nested_field_accesses record (field :: field_names)
      | Pexp_ident ident -> Some (ident :: field_names)
      | _ -> None
    in
    (match flatten_nested_field_accesses record [ field ] with
     | None -> expression
     | Some name_components ->
       let name_prefix =
         match
           List.filter_map name_components ~f:(fun ident ->
             longident_to_human_friendly_string ident.txt)
         with
         | [] -> None
         | name_components -> Some (String.concat name_components ~sep:"_")
       in
       Renaming_env.rename env expression ~name_prefix)
  | _ -> expression
;;

let sexp_of_constraint env ~omit_nil ~stackify ~loc expr ctyp =
  let optional ty =
    let sexp_of = Ppx_sexp_conv_expander.Sexp_of.core_type ty ~stackify in
    Optional
      ( loc
      , rename_if_ident_or_field_access env expr
      , fun expr -> [%expr [%e sexp_of] [%e expr]] )
  in
  match ctyp with
  | [%type: [%t? ty] option] when Option.is_some (Attribute.get option_attr ctyp) ->
    optional ty
  | [%type: [%t? ty] option] when omit_nil -> optional ty
  | _ ->
    let expr =
      let sexp_of = Ppx_sexp_conv_expander.Sexp_of.core_type ctyp ~stackify in
      [%expr [%e sexp_of] [%e rename_if_ident_or_field_access env expr]]
    in
    let omit_nil_attr =
      lazy
        (* this is lazy so using [@omit_nil] inside [%message.omit_nil] is an error (unused
           attribute) *)
        (match Attribute.get omit_nil_attr ctyp with
         | Some () -> true
         | None -> false)
    in
    present_or_omit_nil ~loc expr ~omit_nil:(omit_nil || Lazy.force omit_nil_attr)
;;

let sexp_of_constant ~loc ~stackify const =
  let stackify_suffix = if stackify then "__stack" else "" in
  let mk typ const =
    eapply
      ~loc
      (evar ~loc ("Ppx_sexp_conv_lib.Conv.sexp_of_" ^ typ ^ stackify_suffix))
      [ pexp_constant ~loc const ]
  in
  let f typ = mk typ const in
  match Ppxlib_jane.Shim.Constant.of_parsetree const with
  | Pconst_integer _ -> f "int"
  | Pconst_char _ -> f "char"
  | Pconst_string _ -> f "string"
  | Pconst_float _ -> f "float"
  | Pconst_unboxed_float (x, c) -> mk "float" (Pconst_float (x, c))
  | Pconst_unboxed_integer (x, c) -> mk "int" (Pconst_integer (x, Some c))
  | Pconst_untagged_char c -> mk "char" (Pconst_char c)
;;

let rewrite_here e =
  match e.pexp_desc with
  | Pexp_extension ({ txt = "here"; _ }, PStr []) ->
    Ppx_here_expander.lift_position_as_string ~loc:e.pexp_loc
  | _ -> e
;;

let sexp_of_expr env ~omit_nil ~stackify e =
  let e = rewrite_here e in
  let loc = { e.pexp_loc with loc_ghost = true } in
  match Ppxlib_jane.Shim.Expression_desc.of_parsetree ~loc:e.pexp_loc e.pexp_desc with
  | Pexp_constant (Pconst_string ("", _, _)) -> Absent
  | Pexp_constant const ->
    present_or_omit_nil ~loc ~omit_nil:false (sexp_of_constant ~loc ~stackify const)
  | Pexp_constraint (expr, Some ctyp, _) ->
    sexp_of_constraint env ~omit_nil ~stackify ~loc expr ctyp
  | _ ->
    let e = rename_if_ident_or_field_access env e in
    let sexp_of_string =
      if stackify
      then [%expr Ppx_sexp_conv_lib.Conv.sexp_of_string__stack]
      else [%expr Ppx_sexp_conv_lib.Conv.sexp_of_string]
    in
    present_or_omit_nil ~loc ~omit_nil:false [%expr [%e sexp_of_string] [%e e]]
;;

let sexp_of_labelled_expr env ~omit_nil ~stackify (label, e) =
  let loc = { e.pexp_loc with loc_ghost = true } in
  match
    label, Ppxlib_jane.Shim.Expression_desc.of_parsetree ~loc:e.pexp_loc e.pexp_desc
  with
  | Nolabel, Pexp_constraint (expr, _, _) ->
    let expr_str = Pprintast.string_of_expression expr in
    let k e = sexp_inline ~loc [ sexp_atom ~loc (estring ~loc expr_str); e ] in
    wrap_sexp_if_present (sexp_of_expr ~omit_nil ~stackify env e) ~f:k
  | Nolabel, _ -> sexp_of_expr ~omit_nil ~stackify env e
  | Labelled "_", _ -> sexp_of_expr ~omit_nil ~stackify env e
  | Labelled label, _ ->
    let k e = sexp_inline ~loc [ sexp_atom ~loc (estring ~loc label); e ] in
    wrap_sexp_if_present (sexp_of_expr ~omit_nil ~stackify env e) ~f:k
  | Optional _, _ ->
    (* Could be used to encode sexp_option if that's ever needed. *)
    Location.raise_errorf ~loc "ppx_sexp_value: optional argument not allowed here"
;;

(* Wrap up the generated code in a [@cold] function so it doesn't pollute the call site.

   Also, give that cold function a nice name so it's easy for assembly readers to figure
   out why that function is being called.

   Effectively:

   [%expr
     let[@cold] ppx_sexp_message <parameters> = [%e expr] in
     ppx_sexp_message <arguments> [@nontail]]

   - We avoid allocating the closure in many cases by making [ppx_sexp_message]
     close over as little as possible. Instead, it takes the elements of the
     [%message ...] as arguments.
     + We only avoid closing over identifiers and field accesses (hence
       [rename_if_ident_or_field_access]). That's because the main goal of avoiding the
       closure is to reduce the size of generated code in the caller to improve icache
       behavior, and if the expression is not a simple identifier or field access, then
       evaluating it in the caller may result in more code bloat than simply allocating
       the closure.
   - The [@nontail] lets us *stack* allocate the closure (with Jane Street extensions).
     The sexp this is generating is still heap allocated but it's nice to keep the closure
     allocation cheap where it's easy.
*)
let wrap_in_cold_function ~loc ~parameters ~arguments ~stackify expr =
  let parameters =
    match parameters with
    | [] -> [ [%pat? ()] ]
    | ps -> ps
  in
  let arguments =
    match arguments with
    | [] -> [ [%expr ()] ]
    | args -> args
  in
  let expr = if stackify then [%expr exclave_ [%e expr]] else expr in
  [%expr
    let[@cold] ppx_sexp_message = [%e eabstract ~loc parameters expr] in
    [%e eapply ~loc [%expr ppx_sexp_message] arguments] [@nontail]]
;;

let sexp_of_labelled_exprs ~omit_nil ~stackify ~loc labels_and_exprs =
  let loc = { loc with loc_ghost = true } in
  let env = Renaming_env.create () in
  let l = List.map labels_and_exprs ~f:(sexp_of_labelled_expr env ~omit_nil ~stackify) in
  let res =
    List.fold_left (List.rev l) ~init:(elist ~loc []) ~f:(fun acc e ->
      match e with
      | Absent -> acc
      | Present e -> [%expr [%e e] :: [%e acc]]
      | Optional (_, v_opt, k) ->
        (* We match simultaneously on the head and tail in the generated code to avoid
           changing their respective typing environments. *)
        [%expr
          match [%e v_opt], [%e acc] with
          | None, tl -> tl
          | Some v, tl -> [%e k [%expr v]] :: tl]
      | Omit_nil (_, e, k) ->
        [%expr
          match [%e e], [%e acc] with
          | Ppx_sexp_conv_lib.Sexp.List [], tl -> tl
          | v, tl -> [%e k [%expr v]] :: tl])
  in
  let has_optional_values =
    List.exists l ~f:(function
      | (Optional _ | Omit_nil _ : omittable_sexp) -> true
      | Present _ | Absent -> false)
  in
  (* The two branches do the same thing, but when there are no optional values, we can do
     it at compile-time, which avoids making the generated code ugly. *)
  let final_expr =
    if has_optional_values
    then
      [%expr
        match [%e res] with
        | [ h ] -> h
        | ([] | _ :: _ :: _) as res -> [%e sexp_list ~loc [%expr res]]]
    else (
      match res with
      | [%expr [ [%e? h] ]] -> h
      | _ -> sexp_list ~loc res)
  in
  wrap_in_cold_function
    ~loc
    ~parameters:(Renaming_env.names env)
    ~arguments:(Renaming_env.original_expressions env)
    ~stackify
    final_expr
;;

let expand ~omit_nil ~stackify ~path:_ e =
  let loc = e.pexp_loc in
  let labelled_exprs =
    match e.pexp_desc with
    | Pexp_apply (f, args) -> (Nolabel, f) :: args
    | _ -> [ Nolabel, e ]
  in
  sexp_of_labelled_exprs ~omit_nil ~stackify ~loc labelled_exprs
;;

let expand_opt ~omit_nil ~stackify ~loc ~path opt =
  match opt with
  | None ->
    let loc = { loc with loc_ghost = true } in
    wrap_in_cold_function
      (sexp_list ~loc (elist ~loc []))
      ~loc
      ~parameters:[]
      ~arguments:[]
      ~stackify
  | Some e -> expand ~omit_nil ~stackify ~path e
;;
