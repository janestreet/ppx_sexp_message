open! Core

let eg (_ : Sexp.t) = ()

module Record = struct
  type t =
    { a : int
    ; b : int ref
    }
end

[@@@expand_inline
  [%%template
  [@@@alloc.default a @ m = (heap_global, stack_local)]

  let _ =
    let x = 42
    and y = "forty-two"
    and record : Record.t = { a = 3; b = ref 4 } in
    eg ([%message "foo" 1 2 3 "blah"] [@alloc a]);
    eg ([%message "foo" (x : int) (y : string) (x + String.length y : int)] [@alloc a]);
    eg
      ([%message "foo" (x : int) (y : string) ~blah:(x + String.length y : int)]
      [@alloc a]);
    eg ([%message "foo" ~_:(x : int) ~_:1 ~blah:(0 : int)] [@alloc a]);
    eg ([%message "foo" [%here]] [@alloc a]);
    eg ([%message "foo" ~loc:[%here]] [@alloc a]);
    eg ([%message "foo" ~_:[%here]] [@alloc a]);
    eg ([%message [%here] "blah"] [@alloc a]);
    eg ([%message (sprintf "foo %d" x) (y : string)] [@alloc a]);
    eg ([%message "hello"] [@alloc a]);
    eg ([%message y y] [@alloc a]);
    eg ([%message (sprintf "a") ""] [@alloc a]);
    eg ([%message "" (sprintf "%s" "a")] [@alloc a]);
    eg ([%message [%here]] [@alloc a]);
    eg ([%message [%string "foo"]] [@alloc a]);
    eg ([%message (x : int)] [@alloc a]);
    eg ([%message (x : int) (y : string)] [@alloc a]);
    eg ([%message "" ~_:(x : int) (y : string)] [@alloc a]);
    eg ([%message "foo" ~a:""] [@alloc a]);
    eg ([%message] [@alloc a]);
    eg ([%message (record.a : int) (record.b.contents : int)] [@alloc a]);
    eg
      ([%message
         (Some 1 : (int option[@sexp.option])) (None : (int option[@sexp.option]))]
      [@alloc a]);
    eg
      ([%message
         (This 1 : (int or_null[@sexp.or_null])) (Null : (int or_null[@sexp.or_null]))]
      [@alloc a]);
    eg
      ([%message ([ 1 ] : (int list[@omit_nil])) ([] : (int list[@omit_nil]))] [@alloc a]);
    eg ([%message.omit_nil (Some 1 : int option) (None : int option)] [@alloc a]);
    eg ([%message.omit_nil (This 1 : int or_null) (Null : int or_null)] [@alloc a]);
    eg ([%message.omit_nil ([ 1 ] : int list) ([] : int list)] [@alloc a])
  ;;]

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
    eg
      [%lazy_message
        (This 1 : (int or_null[@sexp.or_null])) (Null : (int or_null[@sexp.or_null]))];
    eg [%lazy_message ([ 1 ] : (int list[@omit_nil])) ([] : (int list[@omit_nil]))];
    eg [%lazy_message.omit_nil (Some 1 : int option) (None : int option)];
    eg [%lazy_message.omit_nil (This 1 : int or_null) (Null : int or_null)];
    eg [%lazy_message.omit_nil ([ 1 ] : int list) ([] : int list)]
  ;;]

include struct
  let _ =
    let x = 42
    and y = "forty-two"
    and record : Record.t = { a = 3; b = ref 4 } in
    eg
      (let ppx_sexp_message () =
         Ppx_sexp_conv_lib.Sexp.List
           [ Ppx_sexp_conv_lib.Conv.sexp_of_string "foo"
           ; Ppx_sexp_conv_lib.Conv.sexp_of_int 1
           ; Ppx_sexp_conv_lib.Conv.sexp_of_int 2
           ; Ppx_sexp_conv_lib.Conv.sexp_of_int 3
           ; Ppx_sexp_conv_lib.Conv.sexp_of_string "blah"
           ]
           [@@cold]
       in
       ppx_sexp_message () [@nontail]);
    eg
      (let ppx_sexp_message x__001_ y__002_ =
         Ppx_sexp_conv_lib.Sexp.List
           [ Ppx_sexp_conv_lib.Conv.sexp_of_string "foo"
           ; Ppx_sexp_conv_lib.Sexp.List
               [ Ppx_sexp_conv_lib.Sexp.Atom "x"; (sexp_of_int [@merlin.hide]) x__001_ ]
           ; Ppx_sexp_conv_lib.Sexp.List
               [ Ppx_sexp_conv_lib.Sexp.Atom "y"
               ; (sexp_of_string [@merlin.hide]) y__002_
               ]
           ; Ppx_sexp_conv_lib.Sexp.List
               [ Ppx_sexp_conv_lib.Sexp.Atom "x + (String.length y)"
               ; (sexp_of_int [@merlin.hide]) (x + String.length y)
               ]
           ]
           [@@cold]
       in
       ppx_sexp_message x y [@nontail]);
    eg
      (let ppx_sexp_message x__003_ y__004_ =
         Ppx_sexp_conv_lib.Sexp.List
           [ Ppx_sexp_conv_lib.Conv.sexp_of_string "foo"
           ; Ppx_sexp_conv_lib.Sexp.List
               [ Ppx_sexp_conv_lib.Sexp.Atom "x"; (sexp_of_int [@merlin.hide]) x__003_ ]
           ; Ppx_sexp_conv_lib.Sexp.List
               [ Ppx_sexp_conv_lib.Sexp.Atom "y"
               ; (sexp_of_string [@merlin.hide]) y__004_
               ]
           ; Ppx_sexp_conv_lib.Sexp.List
               [ Ppx_sexp_conv_lib.Sexp.Atom "blah"
               ; (sexp_of_int [@merlin.hide]) (x + String.length y)
               ]
           ]
           [@@cold]
       in
       ppx_sexp_message x y [@nontail]);
    eg
      (let ppx_sexp_message x__005_ =
         Ppx_sexp_conv_lib.Sexp.List
           [ Ppx_sexp_conv_lib.Conv.sexp_of_string "foo"
           ; (sexp_of_int [@merlin.hide]) x__005_
           ; Ppx_sexp_conv_lib.Conv.sexp_of_int 1
           ; Ppx_sexp_conv_lib.Sexp.List
               [ Ppx_sexp_conv_lib.Sexp.Atom "blah"; (sexp_of_int [@merlin.hide]) 0 ]
           ]
           [@@cold]
       in
       ppx_sexp_message x [@nontail]);
    eg
      (let ppx_sexp_message () =
         Ppx_sexp_conv_lib.Sexp.List
           [ Ppx_sexp_conv_lib.Conv.sexp_of_string "foo"
           ; Ppx_sexp_conv_lib.Conv.sexp_of_string
               "ppx/ppx_sexp_message/test/examples/examples.ml:26:24"
           ]
           [@@cold]
       in
       ppx_sexp_message () [@nontail]);
    eg
      (let ppx_sexp_message () =
         Ppx_sexp_conv_lib.Sexp.List
           [ Ppx_sexp_conv_lib.Conv.sexp_of_string "foo"
           ; Ppx_sexp_conv_lib.Sexp.List
               [ Ppx_sexp_conv_lib.Sexp.Atom "loc"
               ; Ppx_sexp_conv_lib.Conv.sexp_of_string
                   "ppx/ppx_sexp_message/test/examples/examples.ml:27:29"
               ]
           ]
           [@@cold]
       in
       ppx_sexp_message () [@nontail]);
    eg
      (let ppx_sexp_message () =
         Ppx_sexp_conv_lib.Sexp.List
           [ Ppx_sexp_conv_lib.Conv.sexp_of_string "foo"
           ; Ppx_sexp_conv_lib.Conv.sexp_of_string
               "ppx/ppx_sexp_message/test/examples/examples.ml:28:27"
           ]
           [@@cold]
       in
       ppx_sexp_message () [@nontail]);
    eg
      (let ppx_sexp_message () =
         Ppx_sexp_conv_lib.Sexp.List
           [ Ppx_sexp_conv_lib.Conv.sexp_of_string
               "ppx/ppx_sexp_message/test/examples/examples.ml:29:18"
           ; Ppx_sexp_conv_lib.Conv.sexp_of_string "blah"
           ]
           [@@cold]
       in
       ppx_sexp_message () [@nontail]);
    eg
      (let ppx_sexp_message y__006_ =
         Ppx_sexp_conv_lib.Sexp.List
           [ Ppx_sexp_conv_lib.Conv.sexp_of_string (sprintf "foo %d" x)
           ; Ppx_sexp_conv_lib.Sexp.List
               [ Ppx_sexp_conv_lib.Sexp.Atom "y"
               ; (sexp_of_string [@merlin.hide]) y__006_
               ]
           ]
           [@@cold]
       in
       ppx_sexp_message y [@nontail]);
    eg
      (let ppx_sexp_message () = Ppx_sexp_conv_lib.Conv.sexp_of_string "hello" [@@cold] in
       ppx_sexp_message () [@nontail]);
    eg
      (let ppx_sexp_message y__007_ y__008_ =
         Ppx_sexp_conv_lib.Sexp.List
           [ Ppx_sexp_conv_lib.Conv.sexp_of_string y__007_
           ; Ppx_sexp_conv_lib.Conv.sexp_of_string y__008_
           ]
           [@@cold]
       in
       ppx_sexp_message y y [@nontail]);
    eg
      (let ppx_sexp_message () =
         Ppx_sexp_conv_lib.Conv.sexp_of_string (sprintf "a")
           [@@cold]
       in
       ppx_sexp_message () [@nontail]);
    eg
      (let ppx_sexp_message () =
         Ppx_sexp_conv_lib.Conv.sexp_of_string (sprintf "%s" "a")
           [@@cold]
       in
       ppx_sexp_message () [@nontail]);
    eg
      (let ppx_sexp_message () =
         Ppx_sexp_conv_lib.Conv.sexp_of_string
           "ppx/ppx_sexp_message/test/examples/examples.ml:35:18"
           [@@cold]
       in
       ppx_sexp_message () [@nontail]);
    eg
      (let ppx_sexp_message () =
         Ppx_sexp_conv_lib.Conv.sexp_of_string
           (Ppx_string_runtime.For_string__stack__heap.of_string "foo" [@merlin.hide])
           [@@cold]
       in
       ppx_sexp_message () [@nontail]);
    eg
      (let ppx_sexp_message x__009_ =
         Ppx_sexp_conv_lib.Sexp.List
           [ Ppx_sexp_conv_lib.Sexp.Atom "x"; (sexp_of_int [@merlin.hide]) x__009_ ]
           [@@cold]
       in
       ppx_sexp_message x [@nontail]);
    eg
      (let ppx_sexp_message x__010_ y__011_ =
         Ppx_sexp_conv_lib.Sexp.List
           [ Ppx_sexp_conv_lib.Sexp.List
               [ Ppx_sexp_conv_lib.Sexp.Atom "x"; (sexp_of_int [@merlin.hide]) x__010_ ]
           ; Ppx_sexp_conv_lib.Sexp.List
               [ Ppx_sexp_conv_lib.Sexp.Atom "y"
               ; (sexp_of_string [@merlin.hide]) y__011_
               ]
           ]
           [@@cold]
       in
       ppx_sexp_message x y [@nontail]);
    eg
      (let ppx_sexp_message x__012_ y__013_ =
         Ppx_sexp_conv_lib.Sexp.List
           [ (sexp_of_int [@merlin.hide]) x__012_
           ; Ppx_sexp_conv_lib.Sexp.List
               [ Ppx_sexp_conv_lib.Sexp.Atom "y"
               ; (sexp_of_string [@merlin.hide]) y__013_
               ]
           ]
           [@@cold]
       in
       ppx_sexp_message x y [@nontail]);
    eg
      (let ppx_sexp_message () = Ppx_sexp_conv_lib.Conv.sexp_of_string "foo" [@@cold] in
       ppx_sexp_message () [@nontail]);
    eg
      (let ppx_sexp_message () = Ppx_sexp_conv_lib.Sexp.List [] [@@cold] in
       ppx_sexp_message () [@nontail]);
    eg
      (let ppx_sexp_message record_a__014_ record_b_contents__015_ =
         Ppx_sexp_conv_lib.Sexp.List
           [ Ppx_sexp_conv_lib.Sexp.List
               [ Ppx_sexp_conv_lib.Sexp.Atom "record.a"
               ; (sexp_of_int [@merlin.hide]) record_a__014_
               ]
           ; Ppx_sexp_conv_lib.Sexp.List
               [ Ppx_sexp_conv_lib.Sexp.Atom "(record.b).contents"
               ; (sexp_of_int [@merlin.hide]) record_b_contents__015_
               ]
           ]
           [@@cold]
       in
       ppx_sexp_message record.a record.b.contents [@nontail]);
    eg
      (let ppx_sexp_message () =
         match
           match
             ( Some 1
             , match None, [] with
               | None, tl -> tl
               | Some v, tl ->
                 Ppx_sexp_conv_lib.Sexp.List
                   [ Ppx_sexp_conv_lib.Sexp.Atom "None"; (sexp_of_int [@merlin.hide]) v ]
                 :: tl )
           with
           | None, tl -> tl
           | Some v, tl ->
             Ppx_sexp_conv_lib.Sexp.List
               [ Ppx_sexp_conv_lib.Sexp.Atom "Some 1"; (sexp_of_int [@merlin.hide]) v ]
             :: tl
         with
         | h :: [] -> h
         | ([] | _ :: _ :: _) as res -> Ppx_sexp_conv_lib.Sexp.List res
           [@@cold]
       in
       ppx_sexp_message () [@nontail]);
    eg
      (let ppx_sexp_message () =
         match
           match
             ( This 1
             , match Null, [] with
               | Null, tl -> tl
               | This v, tl ->
                 Ppx_sexp_conv_lib.Sexp.List
                   [ Ppx_sexp_conv_lib.Sexp.Atom "Null"; (sexp_of_int [@merlin.hide]) v ]
                 :: tl )
           with
           | Null, tl -> tl
           | This v, tl ->
             Ppx_sexp_conv_lib.Sexp.List
               [ Ppx_sexp_conv_lib.Sexp.Atom "This 1"; (sexp_of_int [@merlin.hide]) v ]
             :: tl
         with
         | h :: [] -> h
         | ([] | _ :: _ :: _) as res -> Ppx_sexp_conv_lib.Sexp.List res
           [@@cold]
       in
       ppx_sexp_message () [@nontail]);
    eg
      (let ppx_sexp_message () =
         match
           match
             ( (fun [@merlin.hide] x__016_ -> sexp_of_list sexp_of_int x__016_) [ 1 ]
             , match
                 (fun [@merlin.hide] x__017_ -> sexp_of_list sexp_of_int x__017_) [], []
               with
               | Ppx_sexp_conv_lib.Sexp.List [], tl -> tl
               | v, tl ->
                 Ppx_sexp_conv_lib.Sexp.List [ Ppx_sexp_conv_lib.Sexp.Atom "[]"; v ] :: tl
             )
           with
           | Ppx_sexp_conv_lib.Sexp.List [], tl -> tl
           | v, tl ->
             Ppx_sexp_conv_lib.Sexp.List [ Ppx_sexp_conv_lib.Sexp.Atom "[1]"; v ] :: tl
         with
         | h :: [] -> h
         | ([] | _ :: _ :: _) as res -> Ppx_sexp_conv_lib.Sexp.List res
           [@@cold]
       in
       ppx_sexp_message () [@nontail]);
    eg
      (let ppx_sexp_message () =
         match
           match
             ( Some 1
             , match None, [] with
               | None, tl -> tl
               | Some v, tl ->
                 Ppx_sexp_conv_lib.Sexp.List
                   [ Ppx_sexp_conv_lib.Sexp.Atom "None"; (sexp_of_int [@merlin.hide]) v ]
                 :: tl )
           with
           | None, tl -> tl
           | Some v, tl ->
             Ppx_sexp_conv_lib.Sexp.List
               [ Ppx_sexp_conv_lib.Sexp.Atom "Some 1"; (sexp_of_int [@merlin.hide]) v ]
             :: tl
         with
         | h :: [] -> h
         | ([] | _ :: _ :: _) as res -> Ppx_sexp_conv_lib.Sexp.List res
           [@@cold]
       in
       ppx_sexp_message () [@nontail]);
    eg
      (let ppx_sexp_message () =
         match
           match
             ( This 1
             , match Null, [] with
               | Null, tl -> tl
               | This v, tl ->
                 Ppx_sexp_conv_lib.Sexp.List
                   [ Ppx_sexp_conv_lib.Sexp.Atom "Null"; (sexp_of_int [@merlin.hide]) v ]
                 :: tl )
           with
           | Null, tl -> tl
           | This v, tl ->
             Ppx_sexp_conv_lib.Sexp.List
               [ Ppx_sexp_conv_lib.Sexp.Atom "This 1"; (sexp_of_int [@merlin.hide]) v ]
             :: tl
         with
         | h :: [] -> h
         | ([] | _ :: _ :: _) as res -> Ppx_sexp_conv_lib.Sexp.List res
           [@@cold]
       in
       ppx_sexp_message () [@nontail]);
    eg
      (let ppx_sexp_message () =
         match
           match
             ( (fun [@merlin.hide] x__018_ -> sexp_of_list sexp_of_int x__018_) [ 1 ]
             , match
                 (fun [@merlin.hide] x__019_ -> sexp_of_list sexp_of_int x__019_) [], []
               with
               | Ppx_sexp_conv_lib.Sexp.List [], tl -> tl
               | v, tl ->
                 Ppx_sexp_conv_lib.Sexp.List [ Ppx_sexp_conv_lib.Sexp.Atom "[]"; v ] :: tl
             )
           with
           | Ppx_sexp_conv_lib.Sexp.List [], tl -> tl
           | v, tl ->
             Ppx_sexp_conv_lib.Sexp.List [ Ppx_sexp_conv_lib.Sexp.Atom "[1]"; v ] :: tl
         with
         | h :: [] -> h
         | ([] | _ :: _ :: _) as res -> Ppx_sexp_conv_lib.Sexp.List res
           [@@cold]
       in
       ppx_sexp_message () [@nontail])
  ;;
end [@@ocaml.doc " @inline "]

include struct
  let _ =
    let x = 42
    and y = "forty-two"
    and record : Record.t = { a = 3; b = ref 4 } in
    eg
      (let ppx_sexp_message () = exclave_
         Ppx_sexp_conv_lib.Sexp.List
           [ Ppx_sexp_conv_lib.Conv.sexp_of_string__stack "foo"
           ; Ppx_sexp_conv_lib.Conv.sexp_of_int__stack 1
           ; Ppx_sexp_conv_lib.Conv.sexp_of_int__stack 2
           ; Ppx_sexp_conv_lib.Conv.sexp_of_int__stack 3
           ; Ppx_sexp_conv_lib.Conv.sexp_of_string__stack "blah"
           ]
           [@@cold]
       in
       ppx_sexp_message () [@nontail]);
    eg
      (let ppx_sexp_message x__020_ y__021_ = exclave_
         Ppx_sexp_conv_lib.Sexp.List
           [ Ppx_sexp_conv_lib.Conv.sexp_of_string__stack "foo"
           ; Ppx_sexp_conv_lib.Sexp.List
               [ Ppx_sexp_conv_lib.Sexp.Atom "x"
               ; (sexp_of_int__stack [@merlin.hide]) x__020_
               ]
           ; Ppx_sexp_conv_lib.Sexp.List
               [ Ppx_sexp_conv_lib.Sexp.Atom "y"
               ; (sexp_of_string__stack [@merlin.hide]) y__021_
               ]
           ; Ppx_sexp_conv_lib.Sexp.List
               [ Ppx_sexp_conv_lib.Sexp.Atom "x + (String.length y)"
               ; (sexp_of_int__stack [@merlin.hide]) (x + String.length y)
               ]
           ]
           [@@cold]
       in
       ppx_sexp_message x y [@nontail]);
    eg
      (let ppx_sexp_message x__022_ y__023_ = exclave_
         Ppx_sexp_conv_lib.Sexp.List
           [ Ppx_sexp_conv_lib.Conv.sexp_of_string__stack "foo"
           ; Ppx_sexp_conv_lib.Sexp.List
               [ Ppx_sexp_conv_lib.Sexp.Atom "x"
               ; (sexp_of_int__stack [@merlin.hide]) x__022_
               ]
           ; Ppx_sexp_conv_lib.Sexp.List
               [ Ppx_sexp_conv_lib.Sexp.Atom "y"
               ; (sexp_of_string__stack [@merlin.hide]) y__023_
               ]
           ; Ppx_sexp_conv_lib.Sexp.List
               [ Ppx_sexp_conv_lib.Sexp.Atom "blah"
               ; (sexp_of_int__stack [@merlin.hide]) (x + String.length y)
               ]
           ]
           [@@cold]
       in
       ppx_sexp_message x y [@nontail]);
    eg
      (let ppx_sexp_message x__024_ = exclave_
         Ppx_sexp_conv_lib.Sexp.List
           [ Ppx_sexp_conv_lib.Conv.sexp_of_string__stack "foo"
           ; (sexp_of_int__stack [@merlin.hide]) x__024_
           ; Ppx_sexp_conv_lib.Conv.sexp_of_int__stack 1
           ; Ppx_sexp_conv_lib.Sexp.List
               [ Ppx_sexp_conv_lib.Sexp.Atom "blah"
               ; (sexp_of_int__stack [@merlin.hide]) 0
               ]
           ]
           [@@cold]
       in
       ppx_sexp_message x [@nontail]);
    eg
      (let ppx_sexp_message () = exclave_
         Ppx_sexp_conv_lib.Sexp.List
           [ Ppx_sexp_conv_lib.Conv.sexp_of_string__stack "foo"
           ; Ppx_sexp_conv_lib.Conv.sexp_of_string__stack
               "ppx/ppx_sexp_message/test/examples/examples.ml:26:24"
           ]
           [@@cold]
       in
       ppx_sexp_message () [@nontail]);
    eg
      (let ppx_sexp_message () = exclave_
         Ppx_sexp_conv_lib.Sexp.List
           [ Ppx_sexp_conv_lib.Conv.sexp_of_string__stack "foo"
           ; Ppx_sexp_conv_lib.Sexp.List
               [ Ppx_sexp_conv_lib.Sexp.Atom "loc"
               ; Ppx_sexp_conv_lib.Conv.sexp_of_string__stack
                   "ppx/ppx_sexp_message/test/examples/examples.ml:27:29"
               ]
           ]
           [@@cold]
       in
       ppx_sexp_message () [@nontail]);
    eg
      (let ppx_sexp_message () = exclave_
         Ppx_sexp_conv_lib.Sexp.List
           [ Ppx_sexp_conv_lib.Conv.sexp_of_string__stack "foo"
           ; Ppx_sexp_conv_lib.Conv.sexp_of_string__stack
               "ppx/ppx_sexp_message/test/examples/examples.ml:28:27"
           ]
           [@@cold]
       in
       ppx_sexp_message () [@nontail]);
    eg
      (let ppx_sexp_message () = exclave_
         Ppx_sexp_conv_lib.Sexp.List
           [ Ppx_sexp_conv_lib.Conv.sexp_of_string__stack
               "ppx/ppx_sexp_message/test/examples/examples.ml:29:18"
           ; Ppx_sexp_conv_lib.Conv.sexp_of_string__stack "blah"
           ]
           [@@cold]
       in
       ppx_sexp_message () [@nontail]);
    eg
      (let ppx_sexp_message y__025_ = exclave_
         Ppx_sexp_conv_lib.Sexp.List
           [ Ppx_sexp_conv_lib.Conv.sexp_of_string__stack (sprintf "foo %d" x)
           ; Ppx_sexp_conv_lib.Sexp.List
               [ Ppx_sexp_conv_lib.Sexp.Atom "y"
               ; (sexp_of_string__stack [@merlin.hide]) y__025_
               ]
           ]
           [@@cold]
       in
       ppx_sexp_message y [@nontail]);
    eg
      (let ppx_sexp_message () = exclave_
         Ppx_sexp_conv_lib.Conv.sexp_of_string__stack "hello"
           [@@cold]
       in
       ppx_sexp_message () [@nontail]);
    eg
      (let ppx_sexp_message y__026_ y__027_ = exclave_
         Ppx_sexp_conv_lib.Sexp.List
           [ Ppx_sexp_conv_lib.Conv.sexp_of_string__stack y__026_
           ; Ppx_sexp_conv_lib.Conv.sexp_of_string__stack y__027_
           ]
           [@@cold]
       in
       ppx_sexp_message y y [@nontail]);
    eg
      (let ppx_sexp_message () = exclave_
         Ppx_sexp_conv_lib.Conv.sexp_of_string__stack (sprintf "a")
           [@@cold]
       in
       ppx_sexp_message () [@nontail]);
    eg
      (let ppx_sexp_message () = exclave_
         Ppx_sexp_conv_lib.Conv.sexp_of_string__stack (sprintf "%s" "a")
           [@@cold]
       in
       ppx_sexp_message () [@nontail]);
    eg
      (let ppx_sexp_message () = exclave_
         Ppx_sexp_conv_lib.Conv.sexp_of_string__stack
           "ppx/ppx_sexp_message/test/examples/examples.ml:35:18"
           [@@cold]
       in
       ppx_sexp_message () [@nontail]);
    eg
      (let ppx_sexp_message () = exclave_
         Ppx_sexp_conv_lib.Conv.sexp_of_string__stack
           (Ppx_string_runtime.For_string__stack__heap.of_string "foo" [@merlin.hide])
           [@@cold]
       in
       ppx_sexp_message () [@nontail]);
    eg
      (let ppx_sexp_message x__028_ = exclave_
         Ppx_sexp_conv_lib.Sexp.List
           [ Ppx_sexp_conv_lib.Sexp.Atom "x"
           ; (sexp_of_int__stack [@merlin.hide]) x__028_
           ]
           [@@cold]
       in
       ppx_sexp_message x [@nontail]);
    eg
      (let ppx_sexp_message x__029_ y__030_ = exclave_
         Ppx_sexp_conv_lib.Sexp.List
           [ Ppx_sexp_conv_lib.Sexp.List
               [ Ppx_sexp_conv_lib.Sexp.Atom "x"
               ; (sexp_of_int__stack [@merlin.hide]) x__029_
               ]
           ; Ppx_sexp_conv_lib.Sexp.List
               [ Ppx_sexp_conv_lib.Sexp.Atom "y"
               ; (sexp_of_string__stack [@merlin.hide]) y__030_
               ]
           ]
           [@@cold]
       in
       ppx_sexp_message x y [@nontail]);
    eg
      (let ppx_sexp_message x__031_ y__032_ = exclave_
         Ppx_sexp_conv_lib.Sexp.List
           [ (sexp_of_int__stack [@merlin.hide]) x__031_
           ; Ppx_sexp_conv_lib.Sexp.List
               [ Ppx_sexp_conv_lib.Sexp.Atom "y"
               ; (sexp_of_string__stack [@merlin.hide]) y__032_
               ]
           ]
           [@@cold]
       in
       ppx_sexp_message x y [@nontail]);
    eg
      (let ppx_sexp_message () = exclave_
         Ppx_sexp_conv_lib.Conv.sexp_of_string__stack "foo"
           [@@cold]
       in
       ppx_sexp_message () [@nontail]);
    eg
      (let ppx_sexp_message () = exclave_ Ppx_sexp_conv_lib.Sexp.List [] [@@cold] in
       ppx_sexp_message () [@nontail]);
    eg
      (let ppx_sexp_message record_a__033_ record_b_contents__034_ = exclave_
         Ppx_sexp_conv_lib.Sexp.List
           [ Ppx_sexp_conv_lib.Sexp.List
               [ Ppx_sexp_conv_lib.Sexp.Atom "record.a"
               ; (sexp_of_int__stack [@merlin.hide]) record_a__033_
               ]
           ; Ppx_sexp_conv_lib.Sexp.List
               [ Ppx_sexp_conv_lib.Sexp.Atom "(record.b).contents"
               ; (sexp_of_int__stack [@merlin.hide]) record_b_contents__034_
               ]
           ]
           [@@cold]
       in
       ppx_sexp_message record.a record.b.contents [@nontail]);
    eg
      (let ppx_sexp_message () = exclave_
         match
           match
             ( Some 1
             , match None, [] with
               | None, tl -> tl
               | Some v, tl ->
                 Ppx_sexp_conv_lib.Sexp.List
                   [ Ppx_sexp_conv_lib.Sexp.Atom "None"
                   ; (sexp_of_int__stack [@merlin.hide]) v
                   ]
                 :: tl )
           with
           | None, tl -> tl
           | Some v, tl ->
             Ppx_sexp_conv_lib.Sexp.List
               [ Ppx_sexp_conv_lib.Sexp.Atom "Some 1"
               ; (sexp_of_int__stack [@merlin.hide]) v
               ]
             :: tl
         with
         | h :: [] -> h
         | ([] | _ :: _ :: _) as res -> Ppx_sexp_conv_lib.Sexp.List res
           [@@cold]
       in
       ppx_sexp_message () [@nontail]);
    eg
      (let ppx_sexp_message () = exclave_
         match
           match
             ( This 1
             , match Null, [] with
               | Null, tl -> tl
               | This v, tl ->
                 Ppx_sexp_conv_lib.Sexp.List
                   [ Ppx_sexp_conv_lib.Sexp.Atom "Null"
                   ; (sexp_of_int__stack [@merlin.hide]) v
                   ]
                 :: tl )
           with
           | Null, tl -> tl
           | This v, tl ->
             Ppx_sexp_conv_lib.Sexp.List
               [ Ppx_sexp_conv_lib.Sexp.Atom "This 1"
               ; (sexp_of_int__stack [@merlin.hide]) v
               ]
             :: tl
         with
         | h :: [] -> h
         | ([] | _ :: _ :: _) as res -> Ppx_sexp_conv_lib.Sexp.List res
           [@@cold]
       in
       ppx_sexp_message () [@nontail]);
    eg
      (let ppx_sexp_message () = exclave_
         match
           match
             ( (fun [@merlin.hide] x__035_ -> exclave_
                 sexp_of_list__stack sexp_of_int__stack x__035_)
                 [ 1 ]
             , match
                 ( (fun [@merlin.hide] x__036_ -> exclave_
                     sexp_of_list__stack sexp_of_int__stack x__036_)
                     []
                 , [] )
               with
               | Ppx_sexp_conv_lib.Sexp.List [], tl -> tl
               | v, tl ->
                 Ppx_sexp_conv_lib.Sexp.List [ Ppx_sexp_conv_lib.Sexp.Atom "[]"; v ] :: tl
             )
           with
           | Ppx_sexp_conv_lib.Sexp.List [], tl -> tl
           | v, tl ->
             Ppx_sexp_conv_lib.Sexp.List [ Ppx_sexp_conv_lib.Sexp.Atom "[1]"; v ] :: tl
         with
         | h :: [] -> h
         | ([] | _ :: _ :: _) as res -> Ppx_sexp_conv_lib.Sexp.List res
           [@@cold]
       in
       ppx_sexp_message () [@nontail]);
    eg
      (let ppx_sexp_message () = exclave_
         match
           match
             ( Some 1
             , match None, [] with
               | None, tl -> tl
               | Some v, tl ->
                 Ppx_sexp_conv_lib.Sexp.List
                   [ Ppx_sexp_conv_lib.Sexp.Atom "None"
                   ; (sexp_of_int__stack [@merlin.hide]) v
                   ]
                 :: tl )
           with
           | None, tl -> tl
           | Some v, tl ->
             Ppx_sexp_conv_lib.Sexp.List
               [ Ppx_sexp_conv_lib.Sexp.Atom "Some 1"
               ; (sexp_of_int__stack [@merlin.hide]) v
               ]
             :: tl
         with
         | h :: [] -> h
         | ([] | _ :: _ :: _) as res -> Ppx_sexp_conv_lib.Sexp.List res
           [@@cold]
       in
       ppx_sexp_message () [@nontail]);
    eg
      (let ppx_sexp_message () = exclave_
         match
           match
             ( This 1
             , match Null, [] with
               | Null, tl -> tl
               | This v, tl ->
                 Ppx_sexp_conv_lib.Sexp.List
                   [ Ppx_sexp_conv_lib.Sexp.Atom "Null"
                   ; (sexp_of_int__stack [@merlin.hide]) v
                   ]
                 :: tl )
           with
           | Null, tl -> tl
           | This v, tl ->
             Ppx_sexp_conv_lib.Sexp.List
               [ Ppx_sexp_conv_lib.Sexp.Atom "This 1"
               ; (sexp_of_int__stack [@merlin.hide]) v
               ]
             :: tl
         with
         | h :: [] -> h
         | ([] | _ :: _ :: _) as res -> Ppx_sexp_conv_lib.Sexp.List res
           [@@cold]
       in
       ppx_sexp_message () [@nontail]);
    eg
      (let ppx_sexp_message () = exclave_
         match
           match
             ( (fun [@merlin.hide] x__037_ -> exclave_
                 sexp_of_list__stack sexp_of_int__stack x__037_)
                 [ 1 ]
             , match
                 ( (fun [@merlin.hide] x__038_ -> exclave_
                     sexp_of_list__stack sexp_of_int__stack x__038_)
                     []
                 , [] )
               with
               | Ppx_sexp_conv_lib.Sexp.List [], tl -> tl
               | v, tl ->
                 Ppx_sexp_conv_lib.Sexp.List [ Ppx_sexp_conv_lib.Sexp.Atom "[]"; v ] :: tl
             )
           with
           | Ppx_sexp_conv_lib.Sexp.List [], tl -> tl
           | v, tl ->
             Ppx_sexp_conv_lib.Sexp.List [ Ppx_sexp_conv_lib.Sexp.Atom "[1]"; v ] :: tl
         with
         | h :: [] -> h
         | ([] | _ :: _ :: _) as res -> Ppx_sexp_conv_lib.Sexp.List res
           [@@cold]
       in
       ppx_sexp_message () [@nontail])
  ;;
end [@@ocaml.doc " @inline "]

let eg (_ : Sexp.t lazy_t) = ()

let () =
  let x = 42
  and y = "forty-two"
  and record : Record.t = { a = 3; b = ref 4 } in
  eg
    (lazy
      (let ppx_sexp_message () =
         Ppx_sexp_conv_lib.Sexp.List
           [ Ppx_sexp_conv_lib.Conv.sexp_of_string "foo"
           ; Ppx_sexp_conv_lib.Conv.sexp_of_int 1
           ; Ppx_sexp_conv_lib.Conv.sexp_of_int 2
           ; Ppx_sexp_conv_lib.Conv.sexp_of_int 3
           ; Ppx_sexp_conv_lib.Conv.sexp_of_string "blah"
           ]
           [@@cold]
       in
       ppx_sexp_message () [@nontail]));
  eg
    (lazy
      (let ppx_sexp_message x__039_ y__040_ =
         Ppx_sexp_conv_lib.Sexp.List
           [ Ppx_sexp_conv_lib.Conv.sexp_of_string "foo"
           ; Ppx_sexp_conv_lib.Sexp.List
               [ Ppx_sexp_conv_lib.Sexp.Atom "x"; (sexp_of_int [@merlin.hide]) x__039_ ]
           ; Ppx_sexp_conv_lib.Sexp.List
               [ Ppx_sexp_conv_lib.Sexp.Atom "y"
               ; (sexp_of_string [@merlin.hide]) y__040_
               ]
           ; Ppx_sexp_conv_lib.Sexp.List
               [ Ppx_sexp_conv_lib.Sexp.Atom "x + (String.length y)"
               ; (sexp_of_int [@merlin.hide]) (x + String.length y)
               ]
           ]
           [@@cold]
       in
       ppx_sexp_message x y [@nontail]));
  eg
    (lazy
      (let ppx_sexp_message x__041_ y__042_ =
         Ppx_sexp_conv_lib.Sexp.List
           [ Ppx_sexp_conv_lib.Conv.sexp_of_string "foo"
           ; Ppx_sexp_conv_lib.Sexp.List
               [ Ppx_sexp_conv_lib.Sexp.Atom "x"; (sexp_of_int [@merlin.hide]) x__041_ ]
           ; Ppx_sexp_conv_lib.Sexp.List
               [ Ppx_sexp_conv_lib.Sexp.Atom "y"
               ; (sexp_of_string [@merlin.hide]) y__042_
               ]
           ; Ppx_sexp_conv_lib.Sexp.List
               [ Ppx_sexp_conv_lib.Sexp.Atom "blah"
               ; (sexp_of_int [@merlin.hide]) (x + String.length y)
               ]
           ]
           [@@cold]
       in
       ppx_sexp_message x y [@nontail]));
  eg
    (lazy
      (let ppx_sexp_message x__043_ =
         Ppx_sexp_conv_lib.Sexp.List
           [ Ppx_sexp_conv_lib.Conv.sexp_of_string "foo"
           ; (sexp_of_int [@merlin.hide]) x__043_
           ; Ppx_sexp_conv_lib.Conv.sexp_of_int 1
           ; Ppx_sexp_conv_lib.Sexp.List
               [ Ppx_sexp_conv_lib.Sexp.Atom "blah"; (sexp_of_int [@merlin.hide]) 0 ]
           ]
           [@@cold]
       in
       ppx_sexp_message x [@nontail]));
  eg
    (lazy
      (let ppx_sexp_message () =
         Ppx_sexp_conv_lib.Sexp.List
           [ Ppx_sexp_conv_lib.Conv.sexp_of_string "foo"
           ; Ppx_sexp_conv_lib.Conv.sexp_of_string
               "ppx/ppx_sexp_message/test/examples/examples.ml:68:28"
           ]
           [@@cold]
       in
       ppx_sexp_message () [@nontail]));
  eg
    (lazy
      (let ppx_sexp_message () =
         Ppx_sexp_conv_lib.Sexp.List
           [ Ppx_sexp_conv_lib.Conv.sexp_of_string "foo"
           ; Ppx_sexp_conv_lib.Sexp.List
               [ Ppx_sexp_conv_lib.Sexp.Atom "loc"
               ; Ppx_sexp_conv_lib.Conv.sexp_of_string
                   "ppx/ppx_sexp_message/test/examples/examples.ml:69:33"
               ]
           ]
           [@@cold]
       in
       ppx_sexp_message () [@nontail]));
  eg
    (lazy
      (let ppx_sexp_message () =
         Ppx_sexp_conv_lib.Sexp.List
           [ Ppx_sexp_conv_lib.Conv.sexp_of_string "foo"
           ; Ppx_sexp_conv_lib.Conv.sexp_of_string
               "ppx/ppx_sexp_message/test/examples/examples.ml:70:31"
           ]
           [@@cold]
       in
       ppx_sexp_message () [@nontail]));
  eg
    (lazy
      (let ppx_sexp_message () =
         Ppx_sexp_conv_lib.Sexp.List
           [ Ppx_sexp_conv_lib.Conv.sexp_of_string
               "ppx/ppx_sexp_message/test/examples/examples.ml:71:22"
           ; Ppx_sexp_conv_lib.Conv.sexp_of_string "blah"
           ]
           [@@cold]
       in
       ppx_sexp_message () [@nontail]));
  eg
    (lazy
      (let ppx_sexp_message y__044_ =
         Ppx_sexp_conv_lib.Sexp.List
           [ Ppx_sexp_conv_lib.Conv.sexp_of_string (sprintf "foo %d" x)
           ; Ppx_sexp_conv_lib.Sexp.List
               [ Ppx_sexp_conv_lib.Sexp.Atom "y"
               ; (sexp_of_string [@merlin.hide]) y__044_
               ]
           ]
           [@@cold]
       in
       ppx_sexp_message y [@nontail]));
  eg
    (lazy
      (let ppx_sexp_message () = Ppx_sexp_conv_lib.Conv.sexp_of_string "hello" [@@cold] in
       ppx_sexp_message () [@nontail]));
  eg
    (lazy
      (let ppx_sexp_message y__045_ y__046_ =
         Ppx_sexp_conv_lib.Sexp.List
           [ Ppx_sexp_conv_lib.Conv.sexp_of_string y__045_
           ; Ppx_sexp_conv_lib.Conv.sexp_of_string y__046_
           ]
           [@@cold]
       in
       ppx_sexp_message y y [@nontail]));
  eg
    (lazy
      (let ppx_sexp_message () =
         Ppx_sexp_conv_lib.Conv.sexp_of_string (sprintf "a")
           [@@cold]
       in
       ppx_sexp_message () [@nontail]));
  eg
    (lazy
      (let ppx_sexp_message () =
         Ppx_sexp_conv_lib.Conv.sexp_of_string (sprintf "%s" "a")
           [@@cold]
       in
       ppx_sexp_message () [@nontail]));
  eg
    (lazy
      (let ppx_sexp_message () =
         Ppx_sexp_conv_lib.Conv.sexp_of_string
           "ppx/ppx_sexp_message/test/examples/examples.ml:77:22"
           [@@cold]
       in
       ppx_sexp_message () [@nontail]));
  eg
    (lazy
      (let ppx_sexp_message () =
         Ppx_sexp_conv_lib.Conv.sexp_of_string
           (Ppx_string_runtime.For_string__stack__heap.of_string "foo" [@merlin.hide])
           [@@cold]
       in
       ppx_sexp_message () [@nontail]));
  eg
    (lazy
      (let ppx_sexp_message x__047_ =
         Ppx_sexp_conv_lib.Sexp.List
           [ Ppx_sexp_conv_lib.Sexp.Atom "x"; (sexp_of_int [@merlin.hide]) x__047_ ]
           [@@cold]
       in
       ppx_sexp_message x [@nontail]));
  eg
    (lazy
      (let ppx_sexp_message x__048_ y__049_ =
         Ppx_sexp_conv_lib.Sexp.List
           [ Ppx_sexp_conv_lib.Sexp.List
               [ Ppx_sexp_conv_lib.Sexp.Atom "x"; (sexp_of_int [@merlin.hide]) x__048_ ]
           ; Ppx_sexp_conv_lib.Sexp.List
               [ Ppx_sexp_conv_lib.Sexp.Atom "y"
               ; (sexp_of_string [@merlin.hide]) y__049_
               ]
           ]
           [@@cold]
       in
       ppx_sexp_message x y [@nontail]));
  eg
    (lazy
      (let ppx_sexp_message x__050_ y__051_ =
         Ppx_sexp_conv_lib.Sexp.List
           [ (sexp_of_int [@merlin.hide]) x__050_
           ; Ppx_sexp_conv_lib.Sexp.List
               [ Ppx_sexp_conv_lib.Sexp.Atom "y"
               ; (sexp_of_string [@merlin.hide]) y__051_
               ]
           ]
           [@@cold]
       in
       ppx_sexp_message x y [@nontail]));
  eg
    (lazy
      (let ppx_sexp_message () = Ppx_sexp_conv_lib.Conv.sexp_of_string "foo" [@@cold] in
       ppx_sexp_message () [@nontail]));
  eg
    (lazy
      (let ppx_sexp_message () = Ppx_sexp_conv_lib.Sexp.List [] [@@cold] in
       ppx_sexp_message () [@nontail]));
  eg
    (lazy
      (let ppx_sexp_message record_a__052_ record_b_contents__053_ =
         Ppx_sexp_conv_lib.Sexp.List
           [ Ppx_sexp_conv_lib.Sexp.List
               [ Ppx_sexp_conv_lib.Sexp.Atom "record.a"
               ; (sexp_of_int [@merlin.hide]) record_a__052_
               ]
           ; Ppx_sexp_conv_lib.Sexp.List
               [ Ppx_sexp_conv_lib.Sexp.Atom "(record.b).contents"
               ; (sexp_of_int [@merlin.hide]) record_b_contents__053_
               ]
           ]
           [@@cold]
       in
       ppx_sexp_message record.a record.b.contents [@nontail]));
  eg
    (lazy
      (let ppx_sexp_message () =
         match
           match
             ( Some 1
             , match None, [] with
               | None, tl -> tl
               | Some v, tl ->
                 Ppx_sexp_conv_lib.Sexp.List
                   [ Ppx_sexp_conv_lib.Sexp.Atom "None"; (sexp_of_int [@merlin.hide]) v ]
                 :: tl )
           with
           | None, tl -> tl
           | Some v, tl ->
             Ppx_sexp_conv_lib.Sexp.List
               [ Ppx_sexp_conv_lib.Sexp.Atom "Some 1"; (sexp_of_int [@merlin.hide]) v ]
             :: tl
         with
         | h :: [] -> h
         | ([] | _ :: _ :: _) as res -> Ppx_sexp_conv_lib.Sexp.List res
           [@@cold]
       in
       ppx_sexp_message () [@nontail]));
  eg
    (lazy
      (let ppx_sexp_message () =
         match
           match
             ( This 1
             , match Null, [] with
               | Null, tl -> tl
               | This v, tl ->
                 Ppx_sexp_conv_lib.Sexp.List
                   [ Ppx_sexp_conv_lib.Sexp.Atom "Null"; (sexp_of_int [@merlin.hide]) v ]
                 :: tl )
           with
           | Null, tl -> tl
           | This v, tl ->
             Ppx_sexp_conv_lib.Sexp.List
               [ Ppx_sexp_conv_lib.Sexp.Atom "This 1"; (sexp_of_int [@merlin.hide]) v ]
             :: tl
         with
         | h :: [] -> h
         | ([] | _ :: _ :: _) as res -> Ppx_sexp_conv_lib.Sexp.List res
           [@@cold]
       in
       ppx_sexp_message () [@nontail]));
  eg
    (lazy
      (let ppx_sexp_message () =
         match
           match
             ( (fun [@merlin.hide] x__054_ -> sexp_of_list sexp_of_int x__054_) [ 1 ]
             , match
                 (fun [@merlin.hide] x__055_ -> sexp_of_list sexp_of_int x__055_) [], []
               with
               | Ppx_sexp_conv_lib.Sexp.List [], tl -> tl
               | v, tl ->
                 Ppx_sexp_conv_lib.Sexp.List [ Ppx_sexp_conv_lib.Sexp.Atom "[]"; v ] :: tl
             )
           with
           | Ppx_sexp_conv_lib.Sexp.List [], tl -> tl
           | v, tl ->
             Ppx_sexp_conv_lib.Sexp.List [ Ppx_sexp_conv_lib.Sexp.Atom "[1]"; v ] :: tl
         with
         | h :: [] -> h
         | ([] | _ :: _ :: _) as res -> Ppx_sexp_conv_lib.Sexp.List res
           [@@cold]
       in
       ppx_sexp_message () [@nontail]));
  eg
    (lazy
      (let ppx_sexp_message () =
         match
           match
             ( Some 1
             , match None, [] with
               | None, tl -> tl
               | Some v, tl ->
                 Ppx_sexp_conv_lib.Sexp.List
                   [ Ppx_sexp_conv_lib.Sexp.Atom "None"; (sexp_of_int [@merlin.hide]) v ]
                 :: tl )
           with
           | None, tl -> tl
           | Some v, tl ->
             Ppx_sexp_conv_lib.Sexp.List
               [ Ppx_sexp_conv_lib.Sexp.Atom "Some 1"; (sexp_of_int [@merlin.hide]) v ]
             :: tl
         with
         | h :: [] -> h
         | ([] | _ :: _ :: _) as res -> Ppx_sexp_conv_lib.Sexp.List res
           [@@cold]
       in
       ppx_sexp_message () [@nontail]));
  eg
    (lazy
      (let ppx_sexp_message () =
         match
           match
             ( This 1
             , match Null, [] with
               | Null, tl -> tl
               | This v, tl ->
                 Ppx_sexp_conv_lib.Sexp.List
                   [ Ppx_sexp_conv_lib.Sexp.Atom "Null"; (sexp_of_int [@merlin.hide]) v ]
                 :: tl )
           with
           | Null, tl -> tl
           | This v, tl ->
             Ppx_sexp_conv_lib.Sexp.List
               [ Ppx_sexp_conv_lib.Sexp.Atom "This 1"; (sexp_of_int [@merlin.hide]) v ]
             :: tl
         with
         | h :: [] -> h
         | ([] | _ :: _ :: _) as res -> Ppx_sexp_conv_lib.Sexp.List res
           [@@cold]
       in
       ppx_sexp_message () [@nontail]));
  eg
    (lazy
      (let ppx_sexp_message () =
         match
           match
             ( (fun [@merlin.hide] x__056_ -> sexp_of_list sexp_of_int x__056_) [ 1 ]
             , match
                 (fun [@merlin.hide] x__057_ -> sexp_of_list sexp_of_int x__057_) [], []
               with
               | Ppx_sexp_conv_lib.Sexp.List [], tl -> tl
               | v, tl ->
                 Ppx_sexp_conv_lib.Sexp.List [ Ppx_sexp_conv_lib.Sexp.Atom "[]"; v ] :: tl
             )
           with
           | Ppx_sexp_conv_lib.Sexp.List [], tl -> tl
           | v, tl ->
             Ppx_sexp_conv_lib.Sexp.List [ Ppx_sexp_conv_lib.Sexp.Atom "[1]"; v ] :: tl
         with
         | h :: [] -> h
         | ([] | _ :: _ :: _) as res -> Ppx_sexp_conv_lib.Sexp.List res
           [@@cold]
       in
       ppx_sexp_message () [@nontail]))
;;

[@@@end]
