(* Wihtout LSP *)
(*
 * SNU 4190.310 Programming Languages
 * Type Checker Skeleton
 *)

open M

type var = string

type typ =
  | TInt
  | TBool
  | TString
  | TPair of typ * typ
  | TLoc of typ
  | TFun of typ * typ
  | TVar of var
  | TUnion of typ list (* for EQ, WRITE *)

type typ_scheme = SimpleTyp of typ | GenTyp of var list * typ
type typ_env = (M.id * typ_scheme) list

let count = ref 0

let new_var () =
  let _ = count := !count + 1 in
  "x_" ^ string_of_int !count

(* Definitions related to free type variable *)

let union_ftv ftv_1 ftv_2 =
  let ftv_1' = List.filter (fun v -> not (List.mem v ftv_2)) ftv_1 in
  ftv_1' @ ftv_2

let sub_ftv ftv_1 ftv_2 = List.filter (fun v -> not (List.mem v ftv_2)) ftv_1

let rec ftv_of_typ : typ -> var list = function
  | TInt | TBool | TString -> []
  | TPair (t1, t2) -> union_ftv (ftv_of_typ t1) (ftv_of_typ t2)
  | TLoc t -> ftv_of_typ t
  | TFun (t1, t2) -> union_ftv (ftv_of_typ t1) (ftv_of_typ t2)
  | TVar v -> [ v ]
  | TUnion ts -> List.flatten (List.map ftv_of_typ ts)

let ftv_of_scheme : typ_scheme -> var list = function
  | SimpleTyp t -> ftv_of_typ t
  | GenTyp (alphas, t) -> sub_ftv (ftv_of_typ t) alphas

let ftv_of_env : typ_env -> var list =
 fun tyenv ->
  List.fold_left
    (fun acc_ftv (id, tyscm) -> union_ftv acc_ftv (ftv_of_scheme tyscm))
    [] tyenv

(* Generalize given type into a type scheme *)
let generalize : typ_env -> typ -> typ_scheme =
 fun tyenv t ->
  let env_ftv = ftv_of_env tyenv in
  let typ_ftv = ftv_of_typ t in
  let ftv = sub_ftv typ_ftv env_ftv in
  if List.length ftv = 0 then SimpleTyp t else GenTyp (ftv, t)

(* Definitions related to substitution *)

type subst = typ -> typ

let empty_subst : subst = fun t -> t

let make_subst : var -> typ -> subst =
 fun x t ->
  let rec subs t' =
    match t' with
    | TVar x' -> if x = x' then t else t'
    | TPair (t1, t2) -> TPair (subs t1, subs t2)
    | TLoc t'' -> TLoc (subs t'')
    | TFun (t1, t2) -> TFun (subs t1, subs t2)
    | TUnion ts -> TUnion (List.map subs ts)
    | TInt | TBool | TString -> t'
  in
  subs

let ( @@ ) s1 s2 t = s1 (s2 t) (* substitution composition *)

let subst_scheme : subst -> typ_scheme -> typ_scheme =
 fun subs tyscm ->
  match tyscm with
  | SimpleTyp t -> SimpleTyp (subs t)
  | GenTyp (alphas, t) ->
      (* S (\all a.t) = \all b.S{a->b}t  (where b is new variable) *)
      let betas = List.map (fun _ -> new_var ()) alphas in
      let s' =
        List.fold_left2
          (fun acc_subst alpha beta ->
            make_subst alpha (TVar beta) @@ acc_subst)
          empty_subst alphas betas
      in
      GenTyp (betas, subs (s' t))

let subst_env : subst -> typ_env -> typ_env =
 fun subs tyenv ->
  List.map (fun (x, tyscm) -> (x, subst_scheme subs tyscm)) tyenv

(* TODO : Implement this function *)

let instantiate : typ_scheme -> typ = function
  | SimpleTyp t -> t
  | GenTyp (vars, t) ->
      let subst =
        List.fold_left
          (fun acc v -> make_subst v (TVar (new_var ())) @@ acc)
          empty_subst vars
      in
      subst t

let rec occurs v t =
  match t with
  | TVar v' -> v = v'
  | TPair (t1, t2) | TFun (t1, t2) -> occurs v t1 || occurs v t2
  | TLoc t1 -> occurs v t1
  | TUnion ts -> List.exists (occurs v) ts
  | _ -> false

let apply_subst (s : subst) (t : typ) : typ = s t

let rec unify (t1 : typ) (t2 : typ) : subst =
  match (t1, t2) with
  | _ when t1 = t2 -> empty_subst
  | TVar v, t | t, TVar v ->
      if occurs v t then raise (M.TypeError ("occurs check failed: " ^ v))
      else make_subst v t
  | TFun (a1, a2), TFun (b1, b2) | TPair (a1, a2), TPair (b1, b2) ->
      let s1 = unify a1 b1 in
      let s2 = unify (apply_subst s1 a2) (apply_subst s1 b2) in
      s2 @@ s1
  | TLoc t1, TLoc t2 -> unify t1 t2
  | TUnion ts, t | t, TUnion ts -> (
      let try_unify candidate = try Some (unify candidate t) with _ -> None in
      match List.find_map try_unify ts with
      | Some subst -> subst
      | None -> raise (M.TypeError "no match in TUnion"))
  | _ -> raise (M.TypeError "cannot unify")

let rec infer (tyenv : typ_env) (exp : M.exp) : typ * subst =
  match exp with
  | VAR x ->
      let scheme =
        try List.assoc x tyenv
        with Not_found -> raise (M.TypeError ("unbound variable: " ^ x))
      in
      (instantiate scheme, empty_subst)
  | CONST (N _) -> (TInt, empty_subst)
  | CONST (B _) -> (TBool, empty_subst)
  | CONST (S _) -> (TString, empty_subst)
  | FN (x, e) ->
      let tv = TVar (new_var ()) in
      let t_body, s_body = infer ((x, SimpleTyp tv) :: tyenv) e in
      (TFun (apply_subst s_body tv, t_body), s_body)
  | APP (e1, e2) ->
      let t1, s1 = infer tyenv e1 in
      let t2, s2 = infer (subst_env s1 tyenv) e2 in
      let tv = TVar (new_var ()) in
      let s3 = unify (apply_subst s2 t1) (TFun (t2, tv)) in
      (apply_subst s3 tv, s3 @@ s2 @@ s1)
  | LET (VAL (x, e1), e2) ->
      let t1, s1 = infer tyenv e1 in
      let generalized = generalize (subst_env s1 tyenv) (apply_subst s1 t1) in
      let t2, s2 = infer ((x, generalized) :: subst_env s1 tyenv) e2 in
      (t2, s2 @@ s1)
  | LET (REC (f, x, body), e2) ->
      let arg_type = TVar (new_var ()) in
      let ret_type = TVar (new_var ()) in
      let f_type = TFun (arg_type, ret_type) in
      let env' = (x, SimpleTyp arg_type) :: (f, SimpleTyp f_type) :: tyenv in
      let t_body, s1 = infer env' body in
      let s2 = unify (apply_subst s1 ret_type) t_body in
      let final_f_type = apply_subst s2 (apply_subst s1 f_type) in
      let generalized = generalize (subst_env (s2 @@ s1) tyenv) final_f_type in
      let t2, s3 = infer ((f, generalized) :: subst_env (s2 @@ s1) tyenv) e2 in
      (t2, s3 @@ s2 @@ s1)
  | IF (e1, e2, e3) ->
      let t1, s1 = infer tyenv e1 in
      let s1' = unify t1 TBool in
      let s1 = s1' @@ s1 in
      let t2, s2 = infer (subst_env s1 tyenv) e2 in
      let t3, s3 = infer (subst_env s2 (subst_env s1 tyenv)) e3 in
      let s4 = unify (apply_subst s3 t2) t3 in
      (apply_subst s4 t3, s4 @@ s3 @@ s2 @@ s1)
  | BOP (op, e1, e2) -> (
      let t1, s1 = infer tyenv e1 in
      let t2, s2 = infer (subst_env s1 tyenv) e2 in
      match op with
      | ADD | SUB ->
          let s3 = unify (apply_subst s2 t1) TInt in
          let s4 = unify (apply_subst s3 t2) TInt in
          (TInt, s4 @@ s3 @@ s2 @@ s1)
      | AND | OR ->
          let s3 = unify (apply_subst s2 t1) TBool in
          let s4 = unify (apply_subst s3 t2) TBool in
          (TBool, s4 @@ s3 @@ s2 @@ s1)
      | EQ -> (
          (* 1) 두 피연산자가 동일한 타입인지 unify *)
          let s3 = unify (apply_subst s2 t1) (apply_subst s2 t2) in
          let resolved = apply_subst s3 (apply_subst s2 t1) in
          (* 2) 정적 의미론: τ = i ∣ b ∣ s ∣ l(any loc) *)
          match resolved with
          | TInt | TBool | TString | TLoc _ ->
              (* OK: 모든 loc 허용 *)
              (TBool, s3 @@ s2 @@ s1)
          | _ -> raise (M.TypeError "EQ 연산에 허용되지 않는 타입")))
  | READ -> (TInt, empty_subst)
  | WRITE e -> (
      (* e의 타입을 추론 *)
      let t, s = infer tyenv e in
      let t' = apply_subst s t in
      (* 정적 의미론: τ = i ∣ b ∣ s 만 허용 *)
      match t' with
      | TInt | TBool | TString -> (t', s)
      | _ -> raise (M.TypeError "WRITE 연산에 허용되지 않는 타입"))
  | MALLOC e ->
      let t, s = infer tyenv e in
      (TLoc t, s)
  | ASSIGN (e1, e2) ->
      let t1, s1 = infer tyenv e1 in
      let t2, s2 = infer (subst_env s1 tyenv) e2 in
      let s3 = unify (apply_subst s2 t1) (TLoc t2) in
      (t2, s3 @@ s2 @@ s1)
  | BANG e ->
      let t, s = infer tyenv e in
      let tv = TVar (new_var ()) in
      let s' = unify t (TLoc tv) in
      (apply_subst s' tv, s' @@ s)
  | SEQ (e1, e2) ->
      let _, s1 = infer tyenv e1 in
      let t2, s2 = infer (subst_env s1 tyenv) e2 in
      (t2, s2 @@ s1)
  | PAIR (e1, e2) ->
      let t1, s1 = infer tyenv e1 in
      let t2, s2 = infer (subst_env s1 tyenv) e2 in
      (TPair (t1, t2), s2 @@ s1)
  | FST e ->
      let t, s = infer tyenv e in
      let tv1 = TVar (new_var ()) in
      let tv2 = TVar (new_var ()) in
      let s' = unify t (TPair (tv1, tv2)) in
      (apply_subst s' tv1, s' @@ s)
  | SND e ->
      let t, s = infer tyenv e in
      let tv1 = TVar (new_var ()) in
      let tv2 = TVar (new_var ()) in
      let s' = unify t (TPair (tv1, tv2)) in
      (apply_subst s' tv2, s' @@ s)

let check (exp : M.exp) : M.typ =
  let tyenv : typ_env = [] in
  let t, s = infer tyenv exp in
  let t = apply_subst s t in
  let rec convert t =
    match t with
    | TInt -> M.TyInt
    | TBool -> M.TyBool
    | TString -> M.TyString
    | TPair (t1, t2) -> M.TyPair (convert t1, convert t2)
    | TLoc t1 -> M.TyLoc (convert t1)
    | TFun _ -> raise (M.TypeError "no function type as result in M")
    | TVar v -> raise (M.TypeError ("Unresolved type var: " ^ v))
    | TUnion _ -> raise (M.TypeError "Union must be resolved before convert")
  in
  convert t
