type crazy3 =
  | NIL
  | ZERO of crazy3
  | ONE of crazy3
  | MONE of crazy3
  | TWO of crazy3
  | MTWO of crazy3

let rec crazy3add : crazy3 * crazy3 -> crazy3 =
 fun (c1, c2) ->
  match (c1, c2) with
  | NIL, _ -> c2
  | _, NIL -> c1
  | ZERO c1', ZERO c2' -> ZERO (crazy3add (c1', c2'))
  | ZERO c1', ONE c2' -> ONE (crazy3add (c1', c2'))
  | ZERO c1', MONE c2' -> MONE (crazy3add (c1', c2'))
  | ZERO c1', TWO c2' -> TWO (crazy3add (c1', c2'))
  | ZERO c1', MTWO c2' -> MTWO (crazy3add (c1', c2'))
  | ONE c1', ZERO c2' -> ONE (crazy3add (c1', c2'))
  | ONE c1', ONE c2' -> TWO (crazy3add (c1', c2'))
  | ONE c1', MONE c2' -> ZERO (crazy3add (c1', c2'))
  | ONE c1', TWO c2' -> ZERO (crazy3add (ONE NIL, crazy3add (c1', c2')))
  | ONE c1', MTWO c2' -> MONE (crazy3add (c1', c2'))
  | MONE c1', ZERO c2' -> MONE (crazy3add (c1', c2'))
  | MONE c1', ONE c2' -> ZERO (crazy3add (c1', c2'))
  | MONE c1', MONE c2' -> MTWO (crazy3add (c1', c2'))
  | MONE c1', TWO c2' -> ONE (crazy3add (c1', c2'))
  | MONE c1', MTWO c2' -> ZERO (crazy3add (MONE NIL, crazy3add (c1', c2')))
  | TWO c1', ZERO c2' -> TWO (crazy3add (c1', c2'))
  | TWO c1', ONE c2' -> ZERO (crazy3add (ONE NIL, crazy3add (c1', c2')))
  | TWO c1', MONE c2' -> ONE (crazy3add (c1', c2'))
  | TWO c1', TWO c2' -> ONE (crazy3add (ONE NIL, crazy3add (c1', c2')))
  | TWO c1', MTWO c2' -> ZERO (crazy3add (c1', c2'))
  | MTWO c1', ZERO c2' -> MTWO (crazy3add (c1', c2'))
  | MTWO c1', ONE c2' -> MONE (crazy3add (c1', c2'))
  | MTWO c1', MONE c2' -> ZERO (crazy3add (MONE NIL, crazy3add (c1', c2')))
  | MTWO c1', TWO c2' -> ZERO (crazy3add (c1', c2'))
  | MTWO c1', MTWO c2' -> MONE (crazy3add (MONE NIL, crazy3add (c1', c2')))
