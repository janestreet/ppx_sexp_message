open! Core

(* This file contains several examples of [%message ...]. The code is ppx expanded and
   then pretty printed into [examples_generated.ml]. This is a poor man's version of
   [@@deriving_inline], which doesn't exist for [%message ...].
*)

let eg (_ : Sexp.t) = ()

module Record = struct
  type t =
    { a : int
    ; b : int ref
    }
end

let () =
  let x = 42
  and y = "forty-two"
  and record : Record.t = { a = 3; b = ref 4 } in
  eg [%message "foo" 1 2 3 "blah"];
  eg [%message "foo" (x : int) (y : string) (x + String.length y : int)];
  eg [%message "foo" (x : int) (y : string) ~blah:(x + String.length y : int)];
  eg [%message "foo" ~_:(x : int) ~_:1 ~blah:(0 : int)];
  eg [%message "foo" [%here]];
  eg [%message "foo" ~loc:[%here]];
  eg [%message "foo" ~_:[%here]];
  eg [%message [%here] "blah"];
  eg [%message (sprintf "foo %d" x) (y : string)];
  eg [%message "hello"];
  eg [%message y y];
  eg [%message (sprintf "a") ""];
  eg [%message "" (sprintf "%s" "a")];
  eg [%message [%here]];
  eg [%message [%string "foo"]];
  eg [%message (x : int)];
  eg [%message (x : int) (y : string)];
  eg [%message "" ~_:(x : int) (y : string)];
  eg [%message "foo" ~a:""];
  eg [%message];
  eg [%message (record.a : int) (record.b.contents : int)];
  eg [%message (Some 1 : (int option[@sexp.option])) (None : (int option[@sexp.option]))];
  eg [%message ([ 1 ] : (int list[@omit_nil])) ([] : (int list[@omit_nil]))];
  eg [%message.omit_nil (Some 1 : int option) (None : int option)];
  eg [%message.omit_nil ([ 1 ] : int list) ([] : int list)]
;;

let eg (_ : Sexp.t lazy_t) = ()

let () =
  let x = 42
  and y = "forty-two"
  and record : Record.t = { a = 3; b = ref 4 } in
  eg [%lazy_message "foo" 1 2 3 "blah"];
  eg [%lazy_message "foo" (x : int) (y : string) (x + String.length y : int)];
  eg [%lazy_message "foo" (x : int) (y : string) ~blah:(x + String.length y : int)];
  eg [%lazy_message "foo" ~_:(x : int) ~_:1 ~blah:(0 : int)];
  eg [%lazy_message "foo" [%here]];
  eg [%lazy_message "foo" ~loc:[%here]];
  eg [%lazy_message "foo" ~_:[%here]];
  eg [%lazy_message [%here] "blah"];
  eg [%lazy_message (sprintf "foo %d" x) (y : string)];
  eg [%lazy_message "hello"];
  eg [%lazy_message y y];
  eg [%lazy_message (sprintf "a") ""];
  eg [%lazy_message "" (sprintf "%s" "a")];
  eg [%lazy_message [%here]];
  eg [%lazy_message [%string "foo"]];
  eg [%lazy_message (x : int)];
  eg [%lazy_message (x : int) (y : string)];
  eg [%lazy_message "" ~_:(x : int) (y : string)];
  eg [%lazy_message "foo" ~a:""];
  eg [%lazy_message];
  eg [%lazy_message (record.a : int) (record.b.contents : int)];
  eg
    [%lazy_message
      (Some 1 : (int option[@sexp.option])) (None : (int option[@sexp.option]))];
  eg [%lazy_message ([ 1 ] : (int list[@omit_nil])) ([] : (int list[@omit_nil]))];
  eg [%lazy_message.omit_nil (Some 1 : int option) (None : int option)];
  eg [%lazy_message.omit_nil ([ 1 ] : int list) ([] : int list)]
;;
