let merge : int list * int list -> int list =
 fun (l1, l2) ->
  let l = l1 @ l2 in
  List.rev (List.sort Stdlib.compare l)
