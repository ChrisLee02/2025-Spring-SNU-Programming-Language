type crazy3 =
  | NIL
  | ZERO of crazy3
  | ONE of crazy3
  | MONE of crazy3
  | TWO of crazy3
  | MTWO of crazy3

let rec crazy3add : crazy3 * crazy3 -> crazy3 =
 fun (c1, c2) ->
  let get_digit_and_next c =
    match c with
    | ZERO next -> (0, next)
    | ONE next -> (1, next)
    | MONE next -> (-1, next)
    | TWO next -> (2, next)
    | MTWO next -> (-2, next)
    | NIL -> assert false
  in

  match (c1, c2) with
  | NIL, _ -> c2
  | _, NIL -> c1
  | _, _ -> (
      let d1, next1 = get_digit_and_next c1 in
      let d2, next2 = get_digit_and_next c2 in
      let sum = Int.add d1 d2 in
      match sum with
      | -4 -> MONE (crazy3add (crazy3add (next1, next2), MONE NIL))
      | -3 -> ZERO (crazy3add (crazy3add (next1, next2), MONE NIL))
      | -2 -> MTWO (crazy3add (next1, next2))
      | -1 -> MONE (crazy3add (next1, next2))
      | 0 -> ZERO (crazy3add (next1, next2))
      | 1 -> ONE (crazy3add (next1, next2))
      | 2 -> TWO (crazy3add (next1, next2))
      | 3 -> ZERO (crazy3add (crazy3add (next1, next2), ONE NIL))
      | 4 -> ONE (crazy3add (crazy3add (next1, next2), ONE NIL))
      | _ -> assert false)
