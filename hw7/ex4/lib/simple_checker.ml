(*
 * SNU 4190.310 Programming Languages
 * Type Checker Skeleton Code
 *)

open M
open Pp

type var = string

(* τ *)
type typ =
  (* ι *)
  | TInt
  | TBool
  | TString
  | TPair of typ * typ
  | TLoc of typ
  (* τ → τ *)
  | TFun of typ * typ
  (* α *)
  | TVar of var
  | TUnion of typ list

(* Modify, or add more if needed *)
type equation = Equal of typ * typ
type subst = (var * typ) list
type type_env = (var * typ) list

let count = ref 0

let new_var () =
  let _ = count := !count + 1 in
  "x_" ^ string_of_int !count

let empty_env : type_env = []
let extend (env : type_env) (x : var) (t : typ) : type_env = (x, t) :: env

let rec lookup (env : type_env) (x : string) : typ =
  match env with
  | [] -> raise (M.TypeError ("unbound variable: " ^ x))
  | (y, t) :: rest -> if x = y then t else lookup rest x

let rec string_typ =
 fun t ->
  match t with
  | TInt -> "int"
  | TBool -> "bool"
  | TString -> "string"
  | TPair (t1, t2) -> "(" ^ string_typ t1 ^ ", " ^ string_typ t2 ^ ")"
  | TLoc t1 -> "loc(" ^ string_typ t1 ^ ")"
  | TFun (t1, t2) -> "(" ^ string_typ t1 ^ " -> " ^ string_typ t2 ^ ")"
  | TVar v -> v
  | TUnion ts ->
      let ts_str = List.map string_typ ts in
      String.concat " + " ts_str

let rec collect_equations : M.exp * type_env -> typ * equation list =
 fun (e, env) ->
  match e with
  | CONST (S _) -> (TString, [])
  | CONST (N _) -> (TInt, [])
  | CONST (B _) -> (TBool, [])
  | VAR id ->
      let typ = lookup env id in
      (typ, [])
  | FN (id, body) ->
      let arg_type = TVar (new_var ()) in
      let body_type, body_conds =
        collect_equations (body, extend env id arg_type)
      in
      let fun_type = TFun (arg_type, body_type) in
      (fun_type, body_conds)
  | APP (fn, arg) ->
      let fn_type, conds1 = collect_equations (fn, env) in
      let arg_type, conds2 = collect_equations (arg, env) in
      let ret_type = TVar (new_var ()) in
      let app_cond = Equal (fn_type, TFun (arg_type, ret_type)) in
      (ret_type, (app_cond :: conds1) @ conds2)
  | LET (VAL (id, e1), e2) ->
      let e1_type, e1_conds = collect_equations (e1, env) in
      let e2_type, e2_conds = collect_equations (e2, extend env id e1_type) in
      (e2_type, e1_conds @ e2_conds)
  | LET (REC (f_arg, arg_id, body), e) ->
      let arg_type = TVar (new_var ()) in
      let ret_type = TVar (new_var ()) in
      let f_type = TFun (arg_type, ret_type) in
      let env' = extend (extend env arg_id arg_type) f_arg f_type in
      let body_type, body_conds = collect_equations (body, env') in
      let env'' = extend env f_arg f_type in
      let e_type, e_conds = collect_equations (e, env'') in
      (e_type, (Equal (body_type, ret_type) :: body_conds) @ e_conds)
  | IF (cond, then_e, else_e) ->
      let cond_type, cond_conds = collect_equations (cond, env) in
      let then_type, then_conds = collect_equations (then_e, env) in
      let else_type, else_conds = collect_equations (else_e, env) in
      let if_bool_cond = Equal (cond_type, TBool) in
      let then_else_equivalent_cond = Equal (then_type, else_type) in
      ( then_type,
        (then_else_equivalent_cond :: if_bool_cond :: then_conds)
        @ else_conds @ cond_conds )
  | BOP (op, left, right) -> (
      let left_type, left_conds = collect_equations (left, env) in
      let right_type, right_conds = collect_equations (right, env) in
      match op with
      | ADD | SUB ->
          let bop_cond = Equal (left_type, TInt) in
          let bop_cond2 = Equal (right_type, TInt) in
          (TInt, (bop_cond :: bop_cond2 :: left_conds) @ right_conds)
      | AND | OR ->
          let bop_cond = Equal (left_type, TBool) in
          let bop_cond2 = Equal (right_type, TBool) in
          (TBool, (bop_cond :: bop_cond2 :: left_conds) @ right_conds)
      | EQ ->
          let bop_cond = Equal (left_type, right_type) in
          let bop_cond2 =
            Equal
              ( left_type,
                TUnion [ TInt; TBool; TString; TLoc (TVar (new_var ())) ] )
          in
          (TBool, (bop_cond :: bop_cond2 :: left_conds) @ right_conds))
  | READ -> (TInt, [])
  | WRITE e ->
      let e_type, e_conds = collect_equations (e, env) in
      (e_type, Equal (e_type, TUnion [ TInt; TBool; TString ]) :: e_conds)
  | MALLOC e ->
      let e_type, e_conds = collect_equations (e, env) in
      let loc_type = TLoc e_type in
      (loc_type, e_conds)
  | ASSIGN (loc, e) ->
      let loc_e_type, loc_e_conds = collect_equations (loc, env) in
      let e_type, e_conds = collect_equations (e, env) in
      (e_type, (Equal (loc_e_type, TLoc e_type) :: loc_e_conds) @ e_conds)
  | BANG loc ->
      let loc_type, loc_conds = collect_equations (loc, env) in
      let deref_type = TVar (new_var ()) in
      let deref_cond = Equal (loc_type, TLoc deref_type) in
      (deref_type, deref_cond :: loc_conds)
  | SEQ (e1, e2) ->
      let _, e1_conds = collect_equations (e1, env) in
      let e2_type, e2_conds = collect_equations (e2, env) in
      (e2_type, e1_conds @ e2_conds)
  | PAIR (e1, e2) ->
      let e1_type, e1_conds = collect_equations (e1, env) in
      let e2_type, e2_conds = collect_equations (e2, env) in
      let pair_type = TPair (e1_type, e2_type) in
      (pair_type, e1_conds @ e2_conds)
  | FST e ->
      let fst_type = TVar (new_var ()) in
      let e_type, e_conds = collect_equations (e, env) in
      let fst_cond = Equal (e_type, TPair (fst_type, TVar (new_var ()))) in
      (fst_type, fst_cond :: e_conds)
  | SND e ->
      let snd_type = TVar (new_var ()) in
      let e_type, e_conds = collect_equations (e, env) in
      let snd_cond = Equal (e_type, TPair (TVar (new_var ()), snd_type)) in
      (snd_type, snd_cond :: e_conds)

let rec apply_subst (s : subst) (t : typ) : typ =
  match t with
  | TInt | TBool | TString -> t
  | TPair (t1, t2) -> TPair (apply_subst s t1, apply_subst s t2)
  | TLoc t1 -> TLoc (apply_subst s t1)
  | TFun (t1, t2) -> TFun (apply_subst s t1, apply_subst s t2)
  | TVar v -> (
      match List.assoc_opt v s with
      | Some t' -> apply_subst s t' (* there can be another Var inside of t' *)
      | None -> TVar v)
  | TUnion ts -> TUnion (List.map (apply_subst s) ts)

let rec occurs v t =
  match t with
  | TVar v' -> v = v'
  | TPair (t1, t2) | TFun (t1, t2) -> occurs v t1 || occurs v t2
  | TLoc t1 -> occurs v t1
  | TUnion ts -> List.exists (occurs v) ts
  | _ -> false

let rec unify (eqs : equation list) (subst : subst) : subst =
  match eqs with
  | [] -> subst
  | Equal (t1, t2) :: rest -> (
      let t1 = apply_subst subst t1 in
      let t2 = apply_subst subst t2 in

      match (t1, t2) with
      | _ when t1 = t2 -> unify rest subst
      | TUnion ts, t | t, TUnion ts -> (
          let try_candidate candidate =
            try Some (unify (Equal (candidate, t) :: rest) subst)
            with _ -> None
          in
          match List.find_map try_candidate ts with
          | Some result -> result
          | None -> raise (M.TypeError "no match in union candidates"))
      | TVar v, t | t, TVar v ->
          if occurs v t then raise (M.TypeError ("occurs check failed: " ^ v))
          else unify rest ((v, t) :: subst)
      | TFun (a1, a2), TFun (b1, b2) | TPair (a1, a2), TPair (b1, b2) ->
          unify (Equal (a1, b1) :: Equal (a2, b2) :: rest) subst
      | TLoc a, TLoc b -> unify (Equal (a, b) :: rest) subst
      | _ -> raise (M.TypeError "cannot unify incompatible types"))

let rec typ_to_mtype t =
  match t with
  | TInt -> M.TyInt
  | TBool -> M.TyBool
  | TString -> M.TyString
  | TPair (t1, t2) -> M.TyPair (typ_to_mtype t1, typ_to_mtype t2)
  | TLoc t1 -> M.TyLoc (typ_to_mtype t1)
  | TFun (t1, t2) -> TyArrow (typ_to_mtype t1, typ_to_mtype t2)
  | TVar v -> raise (M.TypeError ("Unresolved type variable: " ^ v))
  | TUnion _ ->
      raise
        (M.TypeError
           ("Union type must be resolved before conversion type:" ^ string_typ t))

let check : M.exp -> M.types =
 fun exp ->
  let env : type_env = empty_env in
  let root_type, conds = collect_equations (exp, env) in
  let subst = unify conds [] in
  let final_shape = apply_subst subst root_type in
  typ_to_mtype final_shape
