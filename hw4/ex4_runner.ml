open Ex4

(* === ID 프린트 === *)
let string_of_id = function
  | A -> "A"
  | B -> "B"
  | C -> "C"
  | D -> "D"
  | E -> "E"

(* === Gift 리스트 프린트 === *)
let string_of_gift_list gifts =
  "[" ^ String.concat "; " (List.map string_of_int gifts) ^ "]"

(* === 조건 프린트 === *)
let rec string_of_cond = function
  | Items gifts -> "Items " ^ string_of_gift_list gifts
  | Same id -> "Same " ^ string_of_id id
  | Common (c1, c2) ->
      "Common (" ^ string_of_cond c1 ^ ", " ^ string_of_cond c2 ^ ")"
  | Except (c1, c2) ->
      "Except (" ^ string_of_cond c1 ^ ", " ^ string_of_cond c2 ^ ")"

let string_of_cond_list clist =
  "[" ^ String.concat "; " (List.map string_of_cond clist) ^ "]"

(* === require 프린트 === *)
let string_of_require (id, conds) =
  "(" ^ string_of_id id ^ ", " ^ string_of_cond_list conds ^ ")"

let string_of_require_list rlist =
  "[\n  " ^ String.concat ";\n  " (List.map string_of_require rlist) ^ "\n]"

(* === 결과 (id * gift list) 프린트 === *)
let string_of_id_gift_pair (id, gifts) =
  "(" ^ string_of_id id ^ ", " ^ string_of_gift_list gifts ^ ")"

let string_of_id_gift_list lst =
  "[\n  " ^ String.concat ";\n  " (List.map string_of_id_gift_pair lst) ^ "\n]"

(* === 디버깅용 출력 함수 === *)
let print_require_list rlist = print_endline (string_of_require_list rlist)
let print_result r = print_endline (string_of_id_gift_list r)

let test_input1 : require list =
  [
    (A, [ Items [ 1; 2 ]; Common (Same B, Same C) ]);
    (B, [ Common (Same C, Items [ 2; 3 ]) ]);
    (C, [ Items [ 1 ]; Except (Same A, Items []) ]) (* A \ {3} 은 Except *);
  ]

let test_input2 : require list =
  [
    (A, [ Except (Items [ 1; 2 ], Same B) ]);
    (B, [ Except (Same A, Same C) ]);
    (C, [ Common (Items [ 2 ], Same A) ]) (* A \ {3} 은 Except *);
  ]

let example_complex : require list =
  [
    (A, [ Items [ 1; 2 ]; Except (Items [], Same B) ]);
    (* A >= {1,2} \ B *)
    (B, [ Except (Same A, Same C) ]);
    (* B >= A \ C *)
    (C, [ Common (Items [ 2 ], Same A) ]);
    (* C >= {2} ∩ A *)
    (D, []);
    (E, []);
  ]

let example_neg_cycle_simple : require list =
  [ (A, [ Same B ]); (B, [ Same A ]); (C, []); (D, []); (E, []) ]

let example_neg_cycle_except : require list =
  [
    (A, [ Except (Same B, Items [ 1 ]) ]);
    (* A >= B \ {1} *)
    (B, [ Except (Same A, Items [ 2 ]) ]);
    (* B >= A \ {2} *)
    (C, []);
    (D, []);
    (E, []);
  ]

let example2 : require list =
  [
    (A, [ Items [ 1 ]; Except (Items [ 2 ], Same B) ]);
    (B, [ Items [ 2 ]; Except (Items [ 1 ], Same A) ]);
    (C, []);
    (D, []);
    (E, []);
  ]

let example1 : require list =
  [
    (A, [ Items [ 1; 2 ]; Common (Same B, Same C) ]);
    (B, [ Common (Same C, Items [ 2; 3 ]) ]);
    (C, [ Items [ 1 ]; Except (Same A, Items [ 3 ]) ]);
    (D, []);
    (E, []);
  ]

let test_case_abc_strict_fail : require list =
  [
    (A, [ Common (Items [ 1 ], Except (Same B, Same C)) ]);
    (B, [ Common (Items [ 2 ], Except (Same C, Same A)) ]);
    (C, [ Except (Items [ 1; 2 ], Same B) ]);
  ]

let example3 : require list =
  [
    (A, [ Except (Items [ 1 ], Same B) ]); (B, [ Except (Items [ 1 ], Same A) ]);
  ]

(* === 메인 함수 === *)

let () =
  print_endline "=== Input 1 ===";
  print_require_list test_input1;

  print_endline "\n=== Output 1 ===";
  print_result (Ex4.shoppingList test_input1);

  print_endline "=== Input 2 ===";
  print_require_list test_input2;

  print_endline "\n=== Output 2 ===";
  (try print_result (Ex4.shoppingList test_input2)
   with No_minimum -> print_endline "No_minimum");
  print_endline "\n=== Output 3 ===";
  print_result (Ex4.shoppingList example_complex);
  print_endline "\n=== Output 4 ===";
  print_result (Ex4.shoppingList example_neg_cycle_simple);
  print_endline "\n=== Output 5 ===";
  print_result (Ex4.shoppingList example_neg_cycle_except);
  print_endline "\n=== Output 6 ===";
  print_result (Ex4.shoppingList example2);
  print_endline "\n=== Output 7 ===";
  print_result (Ex4.shoppingList example1);
  print_endline "\n=== Output 8 ===";
  print_result (Ex4.shoppingList example3);
  print_endline "\n=== Output 9 ===";
  try print_result (Ex4.shoppingList test_case_abc_strict_fail)
  with No_minimum -> print_endline "No_minimum"
