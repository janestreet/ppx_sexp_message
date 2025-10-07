open! Base
open Ppxlib

let pattern =
  let open Ast_pattern in
  map (single_expr_payload __) ~f:(fun f x -> f (Some x))
  ||| map (pstr nil) ~f:(fun f -> f None)
;;

module Alloc = struct
  type t =
    | Stack
    | Heap
end

module Lazy_or_eager = struct
  type t =
    | Lazy
    (* you can't stackify [Lazy.t]s *)
    | Eager of Alloc.t

  let stackify = function
    | Eager Stack -> true
    | Lazy | Eager Heap -> false
  ;;

  let is_lazy = function
    | Lazy -> true
    | Eager (Stack | Heap) -> false
  ;;
end

let expand ~omit_nil ~lazy_or_eager ~loc ~path expr_opt =
  let stackify = Lazy_or_eager.stackify lazy_or_eager in
  let is_lazy = Lazy_or_eager.is_lazy lazy_or_eager in
  let expr =
    Ppx_sexp_message_expander.expand_opt ~omit_nil ~stackify ~loc ~path expr_opt
  in
  if is_lazy then [%expr lazy [%e expr]] else expr
;;

let message ~name ~omit_nil ~lazy_or_eager =
  Extension.declare
    name
    Extension.Context.expression
    pattern
    (expand ~omit_nil ~lazy_or_eager)
;;

let () =
  Driver.register_transformation
    "sexp_message"
    ~extensions:
      [ message ~name:"message__stack" ~omit_nil:false ~lazy_or_eager:(Eager Stack)
      ; message
          ~name:"@message.omit_nil__stack"
          ~omit_nil:true
          ~lazy_or_eager:(Eager Stack)
      ; message ~name:"message" ~omit_nil:false ~lazy_or_eager:(Eager Heap)
      ; message ~name:"@message.omit_nil" ~omit_nil:true ~lazy_or_eager:(Eager Heap)
      ; message ~name:"lazy_message" ~omit_nil:false ~lazy_or_eager:Lazy
      ; message ~name:"@lazy_message.omit_nil" ~omit_nil:true ~lazy_or_eager:Lazy
      ]
;;
