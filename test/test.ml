open Core

let () = Expect_test_helpers_base.sexp_style := To_string_hum
let pr sexp = Expect_test_helpers_base.print_s ~hide_positions:true sexp

let%expect_test "[%message]" =
  let x = 42
  and y = "forty-two" in
  pr [%message "foo" 1 2 3 "blah"];
  [%expect {| (foo 1 2 3 blah) |}];
  pr [%message "foo" (x : int) (y : string) (x + String.length y : int)];
  [%expect {| (foo (x 42) (y forty-two) ("x + (String.length y)" 51)) |}];
  pr [%message "foo" (x : int) (y : string) ~blah:(x + String.length y : int)];
  [%expect {| (foo (x 42) (y forty-two) (blah 51)) |}];
  pr [%message "foo" ~_:(x : int) ~_:1 ~blah:(0 : int)];
  [%expect {| (foo 42 1 (blah 0)) |}];
  pr [%message "foo" [%here]];
  [%expect {| (foo ppx/ppx_sexp_message/test/test.ml:LINE:COL) |}];
  pr [%message "foo" ~loc:[%here]];
  [%expect {| (foo (loc ppx/ppx_sexp_message/test/test.ml:LINE:COL)) |}];
  pr [%message "foo" ~_:[%here]];
  [%expect {| (foo ppx/ppx_sexp_message/test/test.ml:LINE:COL) |}];
  pr [%message [%here] "blah"];
  [%expect "(ppx/ppx_sexp_message/test/test.ml:LINE:COL blah)"];
  pr [%message (sprintf "foo %d" x) (y : string)];
  [%expect {| ("foo 42" (y forty-two)) |}];
  pr [%message "hello"];
  [%expect {| hello |}];
  pr [%message y y];
  [%expect {| (forty-two forty-two) |}];
  pr [%message (sprintf "a") ""];
  [%expect {| a |}];
  pr [%message "" (sprintf "%s" "a")];
  [%expect {| a |}];
  pr [%message [%here]];
  [%expect {| ppx/ppx_sexp_message/test/test.ml:LINE:COL |}];
  pr [%message [%string "foo"]];
  [%expect {| foo |}];
  pr [%message (x : int)];
  [%expect {| (x 42) |}];
  pr [%message (x : int) (y : string)];
  [%expect {| ((x 42) (y forty-two)) |}];
  pr [%message "" ~_:(x : int) (y : string)];
  [%expect {| (42 (y forty-two)) |}];
  (* This is a bit weird but consistent. *)
  pr [%message "foo" ~a:""];
  [%expect {| foo |}];
  pr [%message];
  [%expect {| () |}];
  pr [%message (Some 1 : (int option[@sexp.option])) (None : (int option[@sexp.option]))];
  [%expect {| ("Some 1" 1) |}];
  pr [%message ([ 1 ] : (int list[@omit_nil])) ([] : (int list[@omit_nil]))];
  [%expect {| ([1] (1)) |}];
  pr [%message.omit_nil (Some 1 : int option) (None : int option)];
  [%expect {| ("Some 1" 1) |}];
  pr [%message.omit_nil ([ 1 ] : int list) ([] : int list)];
  [%expect {| ([1] (1)) |}]
;;

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
  pr_lazy [%lazy_message "" (sprintf "%s" "a")];
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
  (* This is a bit weird but consistent. *)
  pr_lazy [%lazy_message "foo" ~a:""];
  [%expect {| foo |}];
  pr_lazy [%lazy_message];
  [%expect {| () |}];
  pr_lazy
    [%lazy_message
      (Some 1 : (int option[@sexp.option])) (None : (int option[@sexp.option]))];
  [%expect {| ("Some 1" 1) |}];
  pr_lazy [%lazy_message ([ 1 ] : (int list[@omit_nil])) ([] : (int list[@omit_nil]))];
  [%expect {| ([1] (1)) |}];
  pr_lazy [%lazy_message.omit_nil (Some 1 : int option) (None : int option)];
  [%expect {| ("Some 1" 1) |}];
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

    type 'a t = T of 'a [@@deriving sexp_of]
    type 'a not_a_t = Not_t of 'a [@@deriving sexp_of]
  end in
  pr [%message "over values" (T 1 : int t) (Not_t 2 : int not_a_t)];
  pr
    [%message
      "over bits64"
        (T 1L : (Int64_u.t t[@kind bits64]))
        (Not_t 2L : (Int64_u.t not_a_t[@kind bits64]))];
  [%expect
    {|
    ("over values" ("T 1" (T 1)) ("Not_t 2" (Not_t 2)))
    ("over bits64" ("T #1L" (T 1)) ("Not_t #2L" (Not_t 2)))
    |}]
;;
