open Core

let () = Expect_test_helpers_base.sexp_style := To_string_hum
let pr sexp = Expect_test_helpers_base.print_s ~hide_positions:true sexp

let%expect_test _ =
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
  [%expect " (ppx/ppx_sexp_message/test/test.ml:LINE:COL blah) "];
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
