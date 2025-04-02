(*
 * SNU 4190.310 Programming Languages 2025 Spring
 *  K- Interpreter
 *)

(** Location Signature *)
module type LOC = sig
  type t

  val base : t
  val equal : t -> t -> bool
  val diff : t -> t -> int
  val increase : t -> int -> t
end

module Loc : LOC = struct
  type t = Location of int

  let base = Location 0
  let equal (Location a) (Location b) = a = b
  let diff (Location a) (Location b) = a - b
  let increase (Location base) n = Location (base + n)
end

(** Memory Signature *)
module type MEM = sig
  type 'a t

  exception Not_allocated
  exception Not_initialized

  val empty : 'a t
  (** get empty memory *)

  val load : 'a t -> Loc.t -> 'a
  (** load value : Mem.load mem loc => value *)

  val store : 'a t -> Loc.t -> 'a -> 'a t
  (** save value : Mem.store mem loc value => mem' *)

  val alloc : 'a t -> Loc.t * 'a t
  (** get fresh memory cell : Mem.alloc mem => (loc, mem') *)
end

(** Environment Signature *)
module type ENV = sig
  type ('a, 'b) t

  exception Not_bound

  val empty : ('a, 'b) t
  (** get empty environment *)

  val lookup : ('a, 'b) t -> 'a -> 'b
  (** lookup environment : Env.lookup env key => content *)

  val bind : ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t
  (** id binding : Env.bind env key content => env'*)
end

(** Memory Implementation *)
module Mem : MEM = struct
  exception Not_allocated
  exception Not_initialized

  type 'a content = V of 'a | U
  type 'a t = M of Loc.t * 'a content list

  let empty = M (Loc.base, [])

  let rec replace_nth l n c =
    match l with
    | h :: t -> if n = 1 then c :: t else h :: replace_nth t (n - 1) c
    | [] -> raise Not_allocated

  let load (M (boundary, storage)) loc =
    match List.nth storage (Loc.diff boundary loc - 1) with
    | V v -> v
    | U -> raise Not_initialized

  let store (M (boundary, storage)) loc content =
    M (boundary, replace_nth storage (Loc.diff boundary loc) (V content))

  let alloc (M (boundary, storage)) =
    (boundary, M (Loc.increase boundary 1, U :: storage))
end

(** Environment Implementation *)
module Env : ENV = struct
  exception Not_bound

  type ('a, 'b) t = E of ('a -> 'b)

  let empty = E (fun x -> raise Not_bound)
  let lookup (E env) id = env id
  let bind (E env) id loc = E (fun x -> if x = id then loc else env x)
end

(** K- Interpreter *)
module type KMINUS = sig
  exception Error of string

  type id = string

  type exp =
    | NUM of int
    | TRUE
    | FALSE
    | UNIT
    | VAR of id
    | ADD of exp * exp
    | SUB of exp * exp
    | MUL of exp * exp
    | DIV of exp * exp
    | EQUAL of exp * exp
    | LESS of exp * exp
    | NOT of exp
    | SEQ of exp * exp  (** sequence *)
    | IF of exp * exp * exp  (** if-then-else *)
    | WHILE of exp * exp  (** while loop *)
    | LETV of id * exp * exp  (** variable binding *)
    | LETF of id * id list * exp * exp  (** procedure binding *)
    | CALLV of id * exp list  (** call by value *)
    | CALLR of id * id list  (** call by referenece *)
    | RECORD of (id * exp) list  (** record construction *)
    | FIELD of exp * id  (** access record field *)
    | ASSIGN of id * exp  (** assgin to variable *)
    | ASSIGNF of exp * id * exp  (** assign to record field *)
    | READ of id
    | WRITE of exp

  type program = exp
  type memory
  type env
  type value = Num of int | Bool of bool | Unit | Record of (id -> Loc.t)

  val emptyMemory : memory
  val emptyEnv : env
  val run : memory * env * program -> value
end

module K : KMINUS = struct
  exception Error of string
  exception EmptyRecord

  type id = string

  type exp =
    | NUM of int
    | TRUE
    | FALSE
    | UNIT
    | VAR of id
    | ADD of exp * exp
    | SUB of exp * exp
    | MUL of exp * exp
    | DIV of exp * exp
    | EQUAL of exp * exp
    | LESS of exp * exp
    | NOT of exp
    | SEQ of exp * exp  (** sequence *)
    | IF of exp * exp * exp  (** if-then-else *)
    | WHILE of exp * exp  (** while loop *)
    | LETV of id * exp * exp  (** variable binding *)
    | LETF of id * id list * exp * exp  (** procedure binding *)
    | CALLV of id * exp list  (** call by value *)
    | CALLR of id * id list  (** call by referenece *)
    | RECORD of (id * exp) list  (** record construction *)
    | FIELD of exp * id  (** access record field *)
    | ASSIGN of id * exp  (** assgin to variable *)
    | ASSIGNF of exp * id * exp  (** assign to record field *)
    | READ of id
    | WRITE of exp

  type program = exp
  type value = Num of int | Bool of bool | Unit | Record of (id -> Loc.t)
  type memory = value Mem.t

  type env = (id, env_entry) Env.t
  and env_entry = Addr of Loc.t | Proc of id list * exp * env

  let emptyMemory = Mem.empty
  let emptyEnv = Env.empty
  let empty_record : id -> Loc.t = fun x -> raise EmptyRecord

  let value_int v =
    match v with Num n -> n | _ -> raise (Error "TypeError : not int")

  let value_bool v =
    match v with Bool b -> b | _ -> raise (Error "TypeError : not bool")

  let value_unit v =
    match v with Unit -> () | _ -> raise (Error "TypeError : not unit")

  let value_record v =
    match v with Record r -> r | _ -> raise (Error "TypeError : not record")

  let lookup_env_loc e x =
    try
      match Env.lookup e x with
      | Addr l -> l
      | Proc _ -> raise (Error "TypeError : not addr")
    with Env.Not_bound -> raise (Error "Unbound")

  let lookup_env_proc e f =
    try
      match Env.lookup e f with
      | Addr _ -> raise (Error "TypeError : not proc")
      | Proc (id_list, exp, env) -> (id_list, exp, env)
    with Env.Not_bound -> raise (Error "Unbound")

  let rec eval mem env e =
    let parse_two_int e1 e2 mem =
      let v1, mem' = eval mem env e1 in
      let v2, mem'' = eval mem' env e2 in
      let n1 = value_int v1 in
      let n2 = value_int v2 in
      (n1, n2, mem'')
    in

    let map_with_state f init_state lst =
      let result_lst, result_state =
        List.fold_left
          (fun (mapped_list, state) x ->
            let y, new_state = f x state in
            (y :: mapped_list, new_state))
          ([], init_state) lst
      in
      (List.rev result_lst, result_state)
    in

    let eval_with_env e mem = eval mem env e in

    let save_value_mem v mem =
      let l, mem' = Mem.alloc mem in
      (l, Mem.store mem' l v)
    in

    match e with
    | READ x ->
        let v = Num (read_int ()) in
        let l = lookup_env_loc env x in
        (v, Mem.store mem l v)
    | WRITE e ->
        let v, mem' = eval mem env e in
        let n = value_int v in
        let _ = print_endline (string_of_int n) in
        (v, mem')
    | LETV (x, e1, e2) ->
        let v, mem' = eval mem env e1 in
        let l, mem'' = Mem.alloc mem' in
        eval (Mem.store mem'' l v) (Env.bind env x (Addr l)) e2
    | LETF (f, args, fBody, e) ->
        (* Proc of id list * exp * env 로 env_entry를 만들어서 등록  *)
        eval mem (Env.bind env f (Proc (args, fBody, env))) e
    | CALLV (f, e_list) ->
        (* lookup해서 가져오고, args를 돌면서 값들을 바인딩한 후 fBody를 실행하면 됨. *)
        let args, fBody, fEnv = lookup_env_proc env f in
        if List.length args != List.length e_list then
          raise (Error "InvalidArg")
        else
          (* 일단 exp_list를 평가해서 value_list로 만들고. 
             평가하면서 메모리가 변경되니까 이것도 챙겨야함 *)
          (* 그 후에 args랑 value_list를 fold_left2를 돌려서 fEnv에 추가로
              인자들까지 바인딩한 최종 fEnv를 얻고
              거기에 기반해서 fBody를 eval하면 됨.*)
          let v_list, mem' = map_with_state eval_with_env mem e_list in

          let loc_list, mem'' = map_with_state save_value_mem mem' v_list in

          let fEnv_args_binded =
            List.fold_left2
              (fun env arg loc -> Env.bind env arg (Addr loc))
              fEnv args loc_list
          in

          let new_fEnv =
            Env.bind fEnv_args_binded f (Proc (args, fBody, fEnv))
          in

          eval mem'' new_fEnv fBody
    | CALLR (f, ref_list) ->
        let args, fBody, fEnv = lookup_env_proc env f in
        if List.length args != List.length ref_list then
          raise (Error "InvalidArg")
        else
          (* 일단 ref_list를 lookup해서 location_list로 만들면 됨. *)
          (* 그 후에는 동일하다.*)
          let loc_list = List.map (fun x -> lookup_env_loc env x) ref_list in

          let fEnv_args_binded =
            List.fold_left2
              (fun env arg loc -> Env.bind env arg (Addr loc))
              fEnv args loc_list
          in

          let new_fEnv =
            Env.bind fEnv_args_binded f (Proc (args, fBody, fEnv))
          in

          eval mem new_fEnv fBody
    | RECORD key_value_list ->
        (* 메모리 할당을 하고 값을 저장까지 한 후
           주소 리스트를 이용, record 함수를 정의해준다.
        *)
        (* record 함수를 정의하는 방식은 Env.bind 와 유사하게, empty record를 정의하고
            거기다가 함수를 합성해주는 방식으로 가면 될 듯함.
            fold_left2를 활용하자.
        *)
        if List.is_empty key_value_list then (Unit, mem)
        else
          let e_list = List.map (fun (x, y) -> y) key_value_list in

          let id_list = List.map (fun (x, y) -> x) key_value_list in

          let v_list, mem' = map_with_state eval_with_env mem e_list in

          let loc_list, mem'' = map_with_state save_value_mem mem' v_list in

          let record : id -> Loc.t =
            List.fold_left2
              (fun acc id loc -> fun key -> if key = id then loc else acc key)
              empty_record id_list loc_list
          in

          (Record record, mem'')
    | FIELD (record_e, key) -> (
        try
          let record_v, mem' = eval mem env record_e in
          let record = value_record record_v in
          let l = record key in
          let v = Mem.load mem' l in
          (v, mem')
        with EmptyRecord -> raise (Error "Unbound"))
    | ASSIGN (x, e) ->
        let v, mem' = eval mem env e in
        let l = lookup_env_loc env x in
        (v, Mem.store mem' l v)
    | ASSIGNF (target_record_e, key, value_e) -> (
        try
          let value, mem' = eval mem env value_e in
          let record_v, mem'' = eval mem' env target_record_e in
          let record = value_record record_v in
          let l = record key in
          let mem''' = Mem.store mem'' l value in
          (value, mem''')
        with EmptyRecord -> raise (Error "Unbound"))
    | NUM n -> (Num n, mem)
    | TRUE -> (Bool true, mem)
    | FALSE -> (Bool false, mem)
    | UNIT -> (Unit, mem)
    | VAR x ->
        let l = lookup_env_loc env x in
        let v = Mem.load mem l in
        (v, mem)
    | ADD (e1, e2) ->
        let n1, n2, mem'' = parse_two_int e1 e2 mem in
        (Num (n1 + n2), mem'')
    | SUB (e1, e2) ->
        let n1, n2, mem'' = parse_two_int e1 e2 mem in
        (Num (n1 - n2), mem'')
    | MUL (e1, e2) ->
        let n1, n2, mem'' = parse_two_int e1 e2 mem in
        (Num (n1 * n2), mem'')
    | DIV (e1, e2) ->
        let n1, n2, mem'' = parse_two_int e1 e2 mem in
        (Num (n1 / n2), mem'')
    | EQUAL (e1, e2) -> (
        let v1, mem' = eval mem env e1 in
        let v2, mem'' = eval mem' env e2 in
        match (v1, v2) with
        | Num n1, Num n2 -> (Bool (n1 = n2), mem'')
        | Bool b1, Bool b2 -> (Bool (b1 = b2), mem'')
        | Unit, Unit -> (Bool true, mem'')
        | _ -> (Bool false, mem''))
    | LESS (e1, e2) ->
        let n1, n2, mem'' = parse_two_int e1 e2 mem in
        (Bool (n1 < n2), mem'')
    | NOT e1 ->
        let v1, mem' = eval mem env e1 in
        let b1 = value_bool v1 in
        (Bool (not b1), mem')
    | SEQ (e1, e2) ->
        let _, mem' = eval mem env e1 in
        let v2, mem'' = eval mem' env e2 in
        (v2, mem'')
    | IF (econd, et, ee) ->
        let cond_val, mem' = eval mem env econd in
        let cond_b1 = value_bool cond_val in

        if cond_b1 then eval mem' env et else eval mem' env ee
    | WHILE (econd, et) ->
        let cond_val, mem' = eval mem env econd in
        let cond_b1 = value_bool cond_val in

        if cond_b1 then
          let vt, mem'' = eval mem' env et in
          eval mem'' env (WHILE (econd, et))
        else (Unit, mem')

  let run (mem, env, pgm) =
    let v, _ = eval mem env pgm in
    v
end
