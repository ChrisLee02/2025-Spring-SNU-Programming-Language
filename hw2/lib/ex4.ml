module type Queue = sig
  type element
  type queue

  exception EMPTY_Q

  val emptyQ : queue
  val enQ : queue * element -> queue
  val deQ : queue -> element * queue
end

module IntListQ = struct
  type element = int list
  type queue = element list * element list

  exception EMPTY_Q

  let emptyQ = ([], [])

  let enQ : queue * element -> queue =
   fun (q, element) ->
    match q with rear, front -> (element :: rear, front)

  let deQ : queue -> element * queue =
   fun queue ->
    match queue with
    | rear, front -> (
        if List.is_empty rear && List.is_empty front then
          raise EMPTY_Q
        else
          match front with
          | [] -> (
              let rear_rev = List.rev rear in
              match rear_rev with
              | [] -> assert false
              | hd :: tl -> (hd, ([], tl)))
          | hd :: tl -> (hd, (rear, tl)))
end
