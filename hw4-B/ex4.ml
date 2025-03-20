type treasure = StarBox | NameBox of string
type key = Bar | Node of key * key

type map =
  | End of treasure
  | Branch of map * map
  | Guide of string * map

type shape = SBar | SVar of int | SNode of shape * shape
type condition = Equal of shape * shape
type env = (string, shape) Hashtbl.t
type subst = (int * shape) list

exception IMPOSSIBLE

let counter = ref 0

let new_shape_var () : shape =
  incr counter;
  SVar !counter

let rec collect_constraints :
    map * env -> shape * condition list =
 fun (map, env) ->
  match map with
  | End StarBox ->
      if Hashtbl.mem env "*" then (SBar, [])
      else (
        Hashtbl.add env "*" SBar;
        (SBar, []))
  | End (NameBox name) ->
      if Hashtbl.mem env name then
        (Hashtbl.find env name, [])
      else
        let shape_var = new_shape_var () in
        Hashtbl.add env name shape_var;
        (shape_var, [])
  | Branch (map1, map2) ->
      let shape1, conds1 =
        collect_constraints (map1, env)
      in
      let shape2, conds2 =
        collect_constraints (map2, env)
      in
      let shape_var = new_shape_var () in
      let branch_condition =
        Equal (shape1, SNode (shape2, shape_var))
      in
      (shape_var, conds1 @ conds2 @ [ branch_condition ])
  | Guide (name, map) ->
      let shape, conds = collect_constraints (map, env) in
      let shape_var = new_shape_var () in
      let guide_condition =
        Equal
          (shape_var, SNode (Hashtbl.find env name, shape))
      in
      (shape_var, conds @ [ guide_condition ])

let rec occurs n s =
  match s with
  | SBar -> false
  | SVar m -> n = m
  | SNode (s1, s2) -> occurs n s1 || occurs n s2

let rec shape_to_key s =
  match s with
  | SNode (s1, s2) -> Node (shape_to_key s1, shape_to_key s2)
  | _ -> Bar

let rec apply_subst (subst : subst) (s : shape) : shape =
  match s with
  | SBar -> SBar
  | SVar n -> (
      match List.assoc_opt n subst with
      | Some s' -> apply_subst subst s'
      | None -> SVar n)
  | SNode (s1, s2) ->
      SNode (apply_subst subst s1, apply_subst subst s2)

let rec unify (conds : condition list) (subst : subst) :
    subst =
  match conds with
  | [] -> subst
  | Equal (s, t) :: rest -> (
      let s' = apply_subst subst s in
      let t' = apply_subst subst t in
      match (s', t') with
      | _ when s' = t' -> unify rest subst
      | SVar n, _ when not (occurs n t') ->
          unify rest ((n, t') :: subst)
      | _, SVar n when not (occurs n s') ->
          unify rest ((n, s') :: subst)
      | SNode (s1, s2), SNode (t1, t2) ->
          unify
            (Equal (s1, t1) :: Equal (s2, t2) :: rest)
            subst
      | _ -> raise IMPOSSIBLE)

let getReady : map -> key list =
 fun map ->
  let env : env = Hashtbl.create 10 in
  let root_shape, conds = collect_constraints (map, env) in
  let subst = unify conds [] in
  let keys =
    Hashtbl.fold
      (fun _ s acc ->
        let final_shape = apply_subst subst s in
        shape_to_key final_shape :: acc)
      env []
  in
  List.sort_uniq compare keys
