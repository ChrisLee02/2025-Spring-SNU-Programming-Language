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
exception WRONG

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
  | SBar -> Bar
  | SNode (s1, s2) -> Node (shape_to_key s1, shape_to_key s2)
  | SVar _ -> Bar

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
      if s' = t' then unify rest subst
      else
        match (s', t') with
        | SVar n, _ ->
            if occurs n t' then raise IMPOSSIBLE
            else
              let subst' = (n, t') :: subst in
              let rest' =
                List.map
                  (fun (Equal (a, b)) ->
                    Equal
                      ( apply_subst [ (n, t') ] a,
                        apply_subst [ (n, t') ] b ))
                  rest
              in
              unify rest' subst'
        | _, SVar n ->
            if occurs n s' then raise IMPOSSIBLE
            else
              let subst' = (n, s') :: subst in
              let rest' =
                List.map
                  (fun (Equal (a, b)) ->
                    Equal
                      ( apply_subst [ (n, s') ] a,
                        apply_subst [ (n, s') ] b ))
                  rest
              in
              unify rest' subst'
        | SBar, SBar -> unify rest subst
        | SNode (s1, s2), SNode (t1, t2) ->
            unify
              (Equal (s1, t1) :: Equal (s2, t2) :: rest)
              subst
        | _ -> raise IMPOSSIBLE)

(* 자유 변수에 대해 최소 크기 SBar로 기본값 치환 *)
let rec default_free_vars s =
  match s with
  | SVar _ -> SBar
  | SBar -> SBar
  | SNode (s1, s2) ->
      SNode (default_free_vars s1, default_free_vars s2)

let getReady : map -> key list =
 fun map ->
  let env : env = Hashtbl.create 10 in
  let root_shape, conds = collect_constraints (map, env) in
  let subst = unify conds [] in
  if List.is_empty conds then [ Bar ]
  else
    let keys =
      Hashtbl.fold
        (fun _ s acc ->
          let final_shape =
            default_free_vars (apply_subst subst s)
          in
          shape_to_key final_shape :: acc)
        env []
    in
    List.sort_uniq compare keys

(* ---- 디버깅용 출력 함수들 ---- *)

let rec string_of_shape s =
  match s with
  | SBar -> "SBar"
  | SVar n -> "SVar(" ^ string_of_int n ^ ")"
  | SNode (s1, s2) ->
      "SNode(" ^ string_of_shape s1 ^ ", "
      ^ string_of_shape s2 ^ ")"

let string_of_condition (Equal (a, b)) =
  "Equal(" ^ string_of_shape a ^ ", " ^ string_of_shape b
  ^ ")"

let rec string_of_key k =
  match k with
  | Bar -> "Bar"
  | Node (k1, k2) ->
      "Node(" ^ string_of_key k1 ^ ", " ^ string_of_key k2
      ^ ")"

let string_of_subst subst =
  let pairs =
    List.map
      (fun (n, s) ->
        string_of_int n ^ " -> " ^ string_of_shape s)
      subst
  in
  String.concat ", " pairs

(* ---- 테스트 메인 함수 ---- *)
(* getReady_debug: getReady에 디버깅 출력 로직을 추가한 버전 *)
let getReady_debug (map : map) : key list =
  let env : env = Hashtbl.create 10 in
  let root_shape, conds = collect_constraints (map, env) in
  Printf.printf "Root shape: %s\n"
    (string_of_shape root_shape);
  Printf.printf "Constraints:\n";
  List.iter
    (fun cond ->
      Printf.printf "  %s\n" (string_of_condition cond))
    conds;
  let subst =
    try unify conds []
    with IMPOSSIBLE ->
      Printf.printf "Unification failed: IMPOSSIBLE\n";
      []
  in
  Printf.printf "Substitution: %s\n" (string_of_subst subst);
  Printf.printf
    "Env final shapes after substitution and defaulting \
     free vars:\n";
  Hashtbl.iter
    (fun name s ->
      let final_shape =
        default_free_vars (apply_subst subst s)
      in
      Printf.printf "  %s -> %s\n" name
        (string_of_shape final_shape))
    env;
  let keys =
    if List.is_empty conds then [ Bar ]
    else
      let keys =
        Hashtbl.fold
          (fun _ s acc ->
            let final_shape =
              default_free_vars (apply_subst subst s)
            in
            shape_to_key final_shape :: acc)
          env []
      in
      List.sort_uniq compare keys
  in
  Printf.printf "Final keys: ";
  List.iter
    (fun k -> Printf.printf "%s " (string_of_key k))
    keys;
  Printf.printf "\n";
  keys

(* main에서는 단순히 getReady_debug를 호출만 합니다. *)
let main () =
  let test_map =
    Branch (Guide ("x", End (NameBox "x")), End StarBox)
  in
  ignore (getReady_debug test_map)

let () = main ()
