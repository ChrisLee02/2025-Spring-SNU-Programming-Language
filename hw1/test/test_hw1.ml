open Hw1

let () =
  let square x = x * x in
  let sum_of_squares = sigma (1, 3, square) in
  assert (sum_of_squares = 14);

  let identity x = x in
  let sum_of_integers = sigma (1, 5, identity) in
  assert (sum_of_integers = 15);

  let cube x = x * x * x in
  let sum_of_cubes = sigma (1, 4, cube) in
  assert (sum_of_cubes = 100);

  (* iter 테스트 케이스 추가 (덧셈) *)
  let iter_add_test n = iter (n, fun x -> 2 + x) 0 in
  assert (iter_add_test 0 = 0);
  (* iter(0, f) 는 항등함수이므로 0을 반환 *)
  assert (iter_add_test 1 = 2);
  (* iter(1, f) 는 f(0) = 2 *)
  assert (iter_add_test 2 = 4);
  (* iter(2, f) 는 f(f(0)) = f(2) = 4 *)
  assert (iter_add_test 3 = 6);
  (* iter(3, f) 는 f(f(f(0))) = f(4) = 6 *)
  assert (iter_add_test 10 = 20);

  (* iter(10, f) 는 f 10번 적용: 2 * 10 *)

  (* iter 테스트 케이스 추가 (곱셈: 거듭제곱) *)
  let iter_mul_test n = iter (n, fun x -> 2 * x) 1 in
  assert (iter_mul_test 0 = 1);
  (* iter(0, f) 는 항등함수이므로 1을 반환 *)
  assert (iter_mul_test 1 = 2);
  (* iter(1, f) 는 f(1) = 2 *)
  assert (iter_mul_test 2 = 4);
  (* iter(2, f) 는 f(f(1)) = f(2) = 4 *)
  assert (iter_mul_test 3 = 8);
  (* iter(3, f) 는 f(f(f(1))) = f(4) = 8 *)
  assert (iter_mul_test 4 = 16);
  (* iter(4, f) 는 f(f(f(f(1)))) = f(8) = 16 *)
  assert (iter_mul_test 10 = 1024);

  (* iter(10, f) 는 2^10 = 1024 *)
  assert (eval TRUE = true);
  assert (eval FALSE = false);
  assert (eval (NOT TRUE) = false);
  assert (eval (NOT FALSE) = true);

  assert (eval (ANDALSO (TRUE, TRUE)) = true);
  assert (eval (ANDALSO (TRUE, FALSE)) = false);
  assert (eval (ANDALSO (FALSE, TRUE)) = false);
  assert (eval (ANDALSO (FALSE, FALSE)) = false);

  assert (eval (ORELSE (TRUE, TRUE)) = true);
  assert (eval (ORELSE (TRUE, FALSE)) = true);
  assert (eval (ORELSE (FALSE, TRUE)) = true);
  assert (eval (ORELSE (FALSE, FALSE)) = false);

  assert (eval (IMPLY (TRUE, TRUE)) = true);
  assert (eval (IMPLY (TRUE, FALSE)) = false);
  assert (eval (IMPLY (FALSE, TRUE)) = true);
  assert (eval (IMPLY (FALSE, FALSE)) = true);

  (* 수식 연산 테스트 *)
  assert (eval (LESS (NUM 3, NUM 5)) = true);
  assert (eval (LESS (NUM 5, NUM 3)) = false);
  assert (eval (LESS (NUM 3, NUM 3)) = false);

  assert (eval (LESS (PLUS (NUM 2, NUM 3), NUM 6)) = true);
  assert (eval (LESS (PLUS (NUM 2, NUM 3), NUM 5)) = false);

  assert (eval (LESS (MINUS (NUM 7, NUM 4), NUM 3)) = false);
  assert (eval (LESS (MINUS (NUM 7, NUM 4), NUM 4)) = true);

  (* 복합 논리식 테스트 *)
  assert (eval (IMPLY (ANDALSO (TRUE, TRUE), ORELSE (FALSE, TRUE))) = true);
  assert (
    eval (NOT (IMPLY (ANDALSO (TRUE, FALSE), ORELSE (FALSE, FALSE)))) = false);

  print_endline "All tests passed!"
