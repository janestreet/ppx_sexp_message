open Core

let () = Dynamic.set_root Expect_test_helpers_base.sexp_style To_string_hum
let%template[@alloc heap] globalize_sexp sexp = sexp
let%template[@alloc stack] globalize_sexp sexp = Sexp.globalize sexp

[%%template
[@@@alloc.default a @ m = (heap_global, stack_local)]

let pr sexp =
  let sexp = (globalize_sexp [@alloc a]) sexp in
  Expect_test_helpers_base.print_s ~hide_positions:true sexp
;;

let%expect_test "[%message]" =
  let x = 42
  and y = "forty-two" in
  (pr [@alloc a]) ([%message "foo" 1 2 3 "blah"] [@alloc a]);
  [%expect {| (foo 1 2 3 blah) |}];
  (pr [@alloc a])
    ([%message "foo" (x : int) (y : string) (x + String.length y : int)] [@alloc a]);
  [%expect {| (foo (x 42) (y forty-two) ("x + (String.length y)" 51)) |}];
  (pr [@alloc a])
    ([%message "foo" (x : int) (y : string) ~blah:(x + String.length y : int)] [@alloc a]);
  [%expect {| (foo (x 42) (y forty-two) (blah 51)) |}];
  (pr [@alloc a]) ([%message "foo" ~_:(x : int) ~_:1 ~blah:(0 : int)] [@alloc a]);
  [%expect {| (foo 42 1 (blah 0)) |}];
  (pr [@alloc a]) ([%message "foo" [%here]] [@alloc a]);
  [%expect {| (foo ppx/ppx_sexp_message/test/test.ml:LINE:COL) |}];
  (pr [@alloc a]) ([%message "foo" ~loc:[%here]] [@alloc a]);
  [%expect {| (foo (loc ppx/ppx_sexp_message/test/test.ml:LINE:COL)) |}];
  (pr [@alloc a]) ([%message "foo" ~_:[%here]] [@alloc a]);
  [%expect {| (foo ppx/ppx_sexp_message/test/test.ml:LINE:COL) |}];
  (pr [@alloc a]) ([%message [%here] "blah"] [@alloc a]);
  [%expect "(ppx/ppx_sexp_message/test/test.ml:LINE:COL blah)"];
  (pr [@alloc a]) ([%message (sprintf "foo %d" x) (y : string)] [@alloc a]);
  [%expect {| ("foo 42" (y forty-two)) |}];
  (pr [@alloc a]) ([%message "hello"] [@alloc a]);
  [%expect {| hello |}];
  (pr [@alloc a]) ([%message y y] [@alloc a]);
  [%expect {| (forty-two forty-two) |}];
  (pr [@alloc a]) ([%message (sprintf "a") ""] [@alloc a]);
  [%expect {| a |}];
  (pr [@alloc a]) ([%message (sprintf "a") _] [@alloc a]);
  [%expect {| a |}];
  (pr [@alloc a]) ([%message "" (sprintf "%s" "a")] [@alloc a]);
  [%expect {| a |}];
  (pr [@alloc a]) ([%message _ (sprintf "%s" "a")] [@alloc a]);
  [%expect {| a |}];
  (pr [@alloc a]) ([%message [%here]] [@alloc a]);
  [%expect {| ppx/ppx_sexp_message/test/test.ml:LINE:COL |}];
  (pr [@alloc a]) ([%message [%string "foo"]] [@alloc a]);
  [%expect {| foo |}];
  (pr [@alloc a]) ([%message (x : int)] [@alloc a]);
  [%expect {| (x 42) |}];
  (pr [@alloc a]) ([%message (x : int) (y : string)] [@alloc a]);
  [%expect {| ((x 42) (y forty-two)) |}];
  (pr [@alloc a]) ([%message "" ~_:(x : int) (y : string)] [@alloc a]);
  [%expect {| (42 (y forty-two)) |}];
  (pr [@alloc a]) ([%message _ ~_:(x : int) (y : string)] [@alloc a]);
  [%expect {| (42 (y forty-two)) |}];
  (* This is a bit weird but consistent. *)
  (pr [@alloc a]) ([%message "foo" ~a:""] [@alloc a]);
  [%expect {| foo |}];
  (pr [@alloc a]) ([%message "foo" ~a:_] [@alloc a]);
  [%expect {| foo |}];
  (pr [@alloc a]) ([%message "foo" ~_] [@alloc a]);
  [%expect {| foo |}];
  (pr [@alloc a]) ([%message] [@alloc a]);
  [%expect {| () |}];
  (pr [@alloc a])
    ([%message (Some 1 : (int option[@sexp.option])) (None : (int option[@sexp.option]))]
    [@alloc a]);
  [%expect {| ("Some 1" 1) |}];
  (pr [@alloc a])
    ([%message
       (This 1 : (int or_null[@sexp.or_null])) (Null : (int or_null[@sexp.or_null]))]
    [@alloc a]);
  [%expect {| ("This 1" 1) |}];
  (pr [@alloc a])
    ([%message ([ 1 ] : (int list[@omit_nil])) ([] : (int list[@omit_nil]))] [@alloc a]);
  [%expect {| ([1] (1)) |}];
  (pr [@alloc a])
    ([%message.omit_nil (Some 1 : int option) (None : int option)] [@alloc a]);
  [%expect {| ("Some 1" 1) |}];
  (pr [@alloc a])
    ([%message.omit_nil (This 1 : int or_null) (Null : int or_null)] [@alloc a]);
  [%expect {| ("This 1" 1) |}];
  (pr [@alloc a]) ([%message.omit_nil ([ 1 ] : int list) ([] : int list)] [@alloc a]);
  [%expect {| ([1] (1)) |}]
;;]

let pr_lazy lazy_sexp = pr (Lazy.force lazy_sexp)

let%expect_test "[%lazy_message]" =
  let x = 42
  and y = "forty-two" in
  pr_lazy [%lazy_message "foo" 1 2 3 "blah"];
  [%expect {| (foo 1 2 3 blah) |}];
  pr_lazy [%lazy_message "foo" (x : int) (y : string) (x + String.length y : int)];
  [%expect {| (foo (x 42) (y forty-two) ("x + (String.length y)" 51)) |}];
  pr_lazy [%lazy_message "foo" (x : int) (y : string) ~blah:(x + String.length y : int)];
  [%expect {| (foo (x 42) (y forty-two) (blah 51)) |}];
  pr_lazy [%lazy_message "foo" ~_:(x : int) ~_:1 ~blah:(0 : int)];
  [%expect {| (foo 42 1 (blah 0)) |}];
  pr_lazy [%lazy_message "foo" [%here]];
  [%expect {| (foo ppx/ppx_sexp_message/test/test.ml:LINE:COL) |}];
  pr_lazy [%lazy_message "foo" ~loc:[%here]];
  [%expect {| (foo (loc ppx/ppx_sexp_message/test/test.ml:LINE:COL)) |}];
  pr_lazy [%lazy_message "foo" ~_:[%here]];
  [%expect {| (foo ppx/ppx_sexp_message/test/test.ml:LINE:COL) |}];
  pr_lazy [%lazy_message [%here] "blah"];
  [%expect "(ppx/ppx_sexp_message/test/test.ml:LINE:COL blah)"];
  pr_lazy [%lazy_message (sprintf "foo %d" x) (y : string)];
  [%expect {| ("foo 42" (y forty-two)) |}];
  pr_lazy [%lazy_message "hello"];
  [%expect {| hello |}];
  pr_lazy [%lazy_message y y];
  [%expect {| (forty-two forty-two) |}];
  pr_lazy [%lazy_message (sprintf "a") ""];
  [%expect {| a |}];
  pr_lazy [%lazy_message (sprintf "a") _];
  [%expect {| a |}];
  pr_lazy [%lazy_message "" (sprintf "%s" "a")];
  [%expect {| a |}];
  pr_lazy [%lazy_message _ (sprintf "%s" "a")];
  [%expect {| a |}];
  pr_lazy [%lazy_message [%here]];
  [%expect {| ppx/ppx_sexp_message/test/test.ml:LINE:COL |}];
  pr_lazy [%lazy_message [%string "foo"]];
  [%expect {| foo |}];
  pr_lazy [%lazy_message (x : int)];
  [%expect {| (x 42) |}];
  pr_lazy [%lazy_message (x : int) (y : string)];
  [%expect {| ((x 42) (y forty-two)) |}];
  pr_lazy [%lazy_message "" ~_:(x : int) (y : string)];
  [%expect {| (42 (y forty-two)) |}];
  pr_lazy [%lazy_message _ ~_:(x : int) (y : string)];
  [%expect {| (42 (y forty-two)) |}];
  (* This is a bit weird but consistent. *)
  pr_lazy [%lazy_message "foo" ~a:""];
  [%expect {| foo |}];
  pr_lazy [%lazy_message "foo" ~a:_];
  [%expect {| foo |}];
  pr_lazy [%lazy_message "foo" ~_];
  [%expect {| foo |}];
  pr_lazy [%lazy_message];
  [%expect {| () |}];
  pr_lazy
    [%lazy_message
      (Some 1 : (int option[@sexp.option])) (None : (int option[@sexp.option]))];
  [%expect {| ("Some 1" 1) |}];
  pr_lazy
    [%lazy_message
      (This 1 : (int or_null[@sexp.or_null])) (Null : (int or_null[@sexp.or_null]))];
  [%expect {| ("This 1" 1) |}];
  pr_lazy [%lazy_message ([ 1 ] : (int list[@omit_nil])) ([] : (int list[@omit_nil]))];
  [%expect {| ([1] (1)) |}];
  pr_lazy [%lazy_message.omit_nil (Some 1 : int option) (None : int option)];
  [%expect {| ("Some 1" 1) |}];
  pr_lazy [%lazy_message.omit_nil (This 1 : int or_null) (Null : int or_null)];
  [%expect {| ("This 1" 1) |}];
  pr_lazy [%lazy_message.omit_nil ([ 1 ] : int list) ([] : int list)];
  [%expect {| ([1] (1)) |}]
;;

let%expect_test "[%message] is not lazy" =
  let side_effect = ref false in
  let _ = [%message (side_effect := true : unit)] in
  Expect_test_helpers_base.require !side_effect;
  [%expect {| |}]
;;

let%expect_test "[%message_lazy] is lazy" =
  let side_effect = ref false in
  let lazy_message = [%lazy_message (side_effect := true : unit)] in
  Expect_test_helpers_base.require_equal (module Bool) !side_effect false;
  [%expect {| |}];
  let _ = Lazy.force lazy_message in
  Expect_test_helpers_base.require !side_effect;
  [%expect {| |}]
;;

let%expect_test "[%message] works with ppx_template" =
  let open%template struct
    [@@@kind.default k = (bits64, value)]

    type ('a : k) t = T of 'a [@@deriving sexp_of]
    type ('a : k) not_a_t = Not_t of 'a [@@deriving sexp_of]
  end in
  pr [%message "over values" (T 1 : int t) (Not_t 2 : int not_a_t)];
  pr
    [%message
      "over bits64"
        (T #1L : (Int64_u.t t[@kind bits64]))
        (Not_t #2L : (Int64_u.t not_a_t[@kind bits64]))];
  [%expect
    {|
    ("over values" ("T 1" (T 1)) ("Not_t 2" (Not_t 2)))
    ("over bits64" ("T #1L" (T 1)) ("Not_t #2L" (Not_t 2)))
    |}]
;;
