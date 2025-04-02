type require = id * cond list

and cond =
  | Items of gift list (* gifts *)
  | Same of id (* same gifts as for id *)
  | Common of cond * cond (* common gifts *)
  | Except of cond * cond (* exclude gifts *)

and gift = int (* gift id *)
and id = A | B | C | D | E (* pig names *)

exception No_minimum

module IntSet = Set.Make (struct
  type t = int

  let compare = compare
end)

let gift_set_history : (id, IntSet.t list) Hashtbl.t = Hashtbl.create 5
let get_last_element : 'a list -> 'a = fun l -> List.hd (List.rev l)

let from_list_to_set : int list -> IntSet.t =
 fun lst -> List.fold_left (fun acc x -> IntSet.add x acc) IntSet.empty lst

let init_tables () =
  List.iter
    (fun pig -> Hashtbl.add gift_set_history pig [ IntSet.empty ])
    [ A; B; C; D; E ]

let rec eval_cond (c : cond) : IntSet.t =
  match c with
  | Items lst -> from_list_to_set lst
  | Same id -> get_last_element (Hashtbl.find gift_set_history id)
  | Common (c1, c2) ->
      let set1 = eval_cond c1 in
      let set2 = eval_cond c2 in
      IntSet.inter set1 set2
  | Except (c1, c2) ->
      let set1 = eval_cond c1 in
      let set2 = eval_cond c2 in
      IntSet.diff set1 set2

let conds_to_gift_set (conds : cond list) : IntSet.t =
  List.fold_left
    (fun acc cond ->
      let evaluated = eval_cond cond in
      IntSet.union acc evaluated)
    IntSet.empty conds

let rec iter_until_convergence (reqs : require list) : unit =
  let updated = ref false in
  List.iter
    (fun (pig, conds) ->
      let current_set = get_last_element (Hashtbl.find gift_set_history pig) in
      let new_set = conds_to_gift_set conds in
      if not (IntSet.equal current_set new_set) then (
        let pig_history = Hashtbl.find gift_set_history pig in
        if List.mem new_set pig_history then raise No_minimum;
        Hashtbl.replace gift_set_history pig [ new_set ];
        updated := true))
    reqs;
  if !updated then iter_until_convergence reqs

let shoppingList (req_list : require list) : (id * gift list) list =
  init_tables ();
  iter_until_convergence req_list;
  List.map
    (fun pig ->
      ( pig,
        List.sort compare
          (IntSet.elements
             (get_last_element (Hashtbl.find gift_set_history pig))) ))
    [ A; B; C; D; E ]
