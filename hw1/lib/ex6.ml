type crazy3 =
  | NIL
  | ZERO of crazy3
  | ONE of crazy3
  | MONE of crazy3
  | TWO of crazy3
  | MTWO of crazy3

let crazy3val : crazy3 -> int =
 fun crazyNum ->
  let rec crazy3valRec (multiplier, crazyNum) =
    match crazyNum with
    | NIL -> 0
    | ZERO next -> crazy3valRec (multiplier * 3, next)
    | ONE next -> multiplier + crazy3valRec (multiplier * 3, next)
    | MONE next -> (multiplier * -1) + crazy3valRec (multiplier * 3, next)
    | TWO next -> (multiplier * 2) + crazy3valRec (multiplier * 3, next)
    | MTWO next -> (multiplier * -2) + crazy3valRec (multiplier * 3, next)
  in
  crazy3valRec (1, crazyNum)
