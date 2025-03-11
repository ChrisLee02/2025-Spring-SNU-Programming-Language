type item = string
type tree = LEAF of item | NODE of tree list
type zipper = TOP | HAND of tree list * zipper * tree list
type location = LOC of tree * zipper

exception NOMOVE of string

let goLeft loc =
  match loc with
  | LOC (_, TOP) -> raise (NOMOVE "left of top")
  | LOC (t, HAND (l :: left, up, right)) ->
      LOC (l, HAND (left, up, t :: right))
  | LOC (_, HAND ([], _, _)) ->
      raise (NOMOVE "left of first")

let goRight loc =
  match loc with
  | LOC (_, TOP) -> raise (NOMOVE "right of top")
  | LOC (t, HAND (left, up, r :: right)) ->
      LOC (r, HAND (t :: left, up, right))
  | LOC (_, HAND (_, _, [])) ->
      raise (NOMOVE "right of last")

let goUp loc =
  match loc with
  | LOC (_, TOP) -> raise (NOMOVE "up of top")
  | LOC (t, HAND (left, up, right)) ->
      LOC (NODE (List.rev left @ [ t ] @ right), up)

let goDown loc =
  (* go to leftist of child nodes *)
  match loc with
  | LOC (LEAF _, _) -> raise (NOMOVE "down of Leaf")
  | LOC (NODE [], _) ->
      raise (NOMOVE "down of empty children list")
  | LOC (NODE (first :: child_list), zipper) ->
      let up = zipper in
      let zipper = HAND ([], up, child_list) in
      LOC (first, zipper)
