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
  | TUnion of typ list

let rec string_of_typ t =
  match t with
  | TInt -> "int"
  | TBool -> "bool"
  | TString -> "string"
  | TVar v -> "'" ^ v
  | TPair (t1, t2) -> "(" ^ string_of_typ t1 ^ " * " ^ string_of_typ t2 ^ ")"
  | TLoc t1 -> string_of_typ t1 ^ " loc"
  | TFun (t1, t2) -> "(" ^ string_of_typ t1 ^ " -> " ^ string_of_typ t2 ^ ")"
  | TUnion ts ->
      "{"
      ^ String.concat ", " (List.map string_of_typ ts)
      ^ "}" (* Union type *)

type typ_scheme = SimpleTyp of typ | GenTyp of (var list * typ)
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
  | TUnion ts ->
      List.fold_left (fun acc t -> union_ftv acc (ftv_of_typ t)) [] ts

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
  (* print_endline ("Generalizing type: " ^ string_of_typ t); *)
  let env_ftv = ftv_of_env tyenv in
  let typ_ftv = ftv_of_typ t in
  let ftv = sub_ftv typ_ftv env_ftv in
  (* print_endline ("Free type variables: " ^ String.concat ", " ftv); *)
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

let instantiate : typ_scheme -> typ =
 fun typ_scheme ->
  match typ_scheme with
  | SimpleTyp t -> t
  | GenTyp (alphas, t) ->
      let instantiated_vars = List.map (fun _ -> new_var ()) alphas in
      let s =
        List.fold_left2
          (fun acc_subst alpha instantiated_var ->
            make_subst alpha (TVar instantiated_var) @@ acc_subst)
          empty_subst alphas instantiated_vars
      in
      s t

(* Type checking function *)

let rec occurs (v : var) (t : typ) : bool =
  match t with
  | TVar v' -> v = v'
  | TPair (t1, t2) | TFun (t1, t2) -> occurs v t1 || occurs v t2
  | TLoc t1 -> occurs v t1
  | TUnion ts -> List.exists (occurs v) ts
  | TInt | TBool | TString -> false

let rec unify (t1 : typ) (t2 : typ) : subst =
  match (t1, t2) with
  | _ when t1 = t2 -> empty_subst
  | TVar v, t | t, TVar v ->
      if occurs v t then raise (M.TypeError "unification failed")
      else make_subst v t
  | TUnion ts, t | t, TUnion ts ->
      let rec try_unify = function
        | [] -> raise (M.TypeError "union unification failed")
        | hd :: tl -> ( try unify hd t with M.TypeError _ -> try_unify tl)
      in
      try_unify ts
  | TFun (a1, a2), TFun (b1, b2) | TPair (a1, a2), TPair (b1, b2) ->
      let s1 = unify a1 b1 in
      let s2 = unify (s1 a2) (s1 b2) in
      s2 @@ s1
  | TLoc t1, TLoc t2 -> unify t1 t2
  | _ -> raise (M.TypeError "unification failed")

let rec is_expansive (exp : M.exp) : bool =
  match exp with
  | M.CONST _ | M.VAR _ | M.FN _ | M.READ -> false
  | M.APP _ | M.MALLOC _ -> true
  | M.IF (e1, e2, e3) -> is_expansive e1 || is_expansive e2 || is_expansive e3
  | M.LET (REC (_, _, e1), e2) -> is_expansive e1 || is_expansive e2
  | M.LET (VAL (_, e1), e2) -> is_expansive e1 || is_expansive e2
  | M.BOP (_, e1, e2) -> is_expansive e1 || is_expansive e2
  | M.ASSIGN (e1, e2) -> is_expansive e1 || is_expansive e2
  | M.SEQ (e1, e2) -> is_expansive e1 || is_expansive e2
  | M.PAIR (e1, e2) -> is_expansive e1 || is_expansive e2
  | M.WRITE e -> is_expansive e
  | M.BANG e -> is_expansive e
  | M.FST e -> is_expansive e
  | M.SND e -> is_expansive e

let rec infer (tyenv : typ_env) (exp : M.exp) : typ * subst =
  match exp with
  | M.CONST const -> (
      match const with
      | M.N _ -> (TInt, empty_subst)
      | M.B _ -> (TBool, empty_subst)
      | M.S _ -> (TString, empty_subst))
  | M.VAR x ->
      let typ_scheme =
        try List.assoc x tyenv
        with Not_found -> raise (M.TypeError "unbound variable")
      in
      (instantiate typ_scheme, empty_subst)
  | M.FN (x, e) ->
      let tv = TVar (new_var ()) in
      let t_body, s_body = infer ((x, SimpleTyp tv) :: tyenv) e in
      (TFun (s_body tv, s_body t_body), s_body)
  | M.APP (e1, e2) ->
      let t1, s1 = infer tyenv e1 in
      let t2, s2 = infer (subst_env s1 tyenv) e2 in
      let tv = TVar (new_var ()) in
      let s3 = unify (s2 t1) (TFun (t2, tv)) in
      ((s3 @@ s2 @@ s1) tv, s3 @@ s2 @@ s1)
  | M.LET (M.VAL (x, e1), e2) ->
      let t1, s1 = infer tyenv e1 in
      let is_exp = is_expansive e1 in
      let generalized =
        if not is_exp then generalize (subst_env s1 tyenv) (s1 t1)
        else SimpleTyp (s1 t1)
      in
      let new_env = (x, generalized) :: subst_env s1 tyenv in
      let t2, s2 = infer new_env e2 in
      ((s2 @@ s1) t2, s2 @@ s1)
  | M.LET (M.REC (f, x, e1), e2) ->
      let arg_type = TVar (new_var ()) in
      let ret_type = TVar (new_var ()) in
      let f_type = TFun (arg_type, ret_type) in
      let env' = (f, SimpleTyp f_type) :: (x, SimpleTyp arg_type) :: tyenv in
      let t_body, s1 = infer env' e1 in
      let s2 = unify (s1 ret_type) t_body in
      let final_f_type = s2 (s1 f_type) in
      let generalized = generalize (subst_env (s2 @@ s1) tyenv) final_f_type in
      let new_env = (f, generalized) :: subst_env (s2 @@ s1) tyenv in
      let t2, s3 = infer new_env e2 in
      ((s3 @@ s2 @@ s1) t2, s3 @@ s2 @@ s1)
  | M.IF (e1, e2, e3) ->
      let t1, s1 = infer tyenv e1 in
      let s1' = unify t1 TBool in
      let s1 = s1' @@ s1 in
      let t2, s2 = infer (subst_env s1 tyenv) e2 in
      let t3, s3 = infer (subst_env (s2 @@ s1) tyenv) e3 in
      let s4 = unify (s3 t2) t3 in
      ((s4 @@ s3 @@ s2 @@ s1) t3, s4 @@ s3 @@ s2 @@ s1)
  | M.BOP (op, e1, e2) -> (
      let t1, s1 = infer tyenv e1 in
      let t2, s2 = infer (subst_env s1 tyenv) e2 in
      match op with
      | M.ADD | M.SUB ->
          let s3 = unify (s2 t1) TInt in
          let s4 = unify (s3 t2) TInt in
          (TInt, s4 @@ s3 @@ s2 @@ s1)
      | M.AND | M.OR ->
          let s3 = unify (s2 t1) TBool in
          let s4 = unify (s3 t2) TBool in
          (TBool, s4 @@ s3 @@ s2 @@ s1)
      | M.EQ ->
          let s3 = unify (s2 t1) (s2 t2) in
          let allowed =
            TUnion [ TInt; TBool; TString; TLoc (TVar (new_var ())) ]
          in
          let s4 = unify (s3 (s2 t1)) allowed in
          (TBool, s4 @@ s3 @@ s2 @@ s1))
  | M.READ -> (TInt, empty_subst)
  | M.WRITE e ->
      let t, s = infer tyenv e in
      let allowed = TUnion [ TInt; TBool; TString ] in
      let s' = unify (s t) allowed in
      ((s' @@ s) t, s' @@ s)
  | M.MALLOC e ->
      let t, s = infer tyenv e in
      (TLoc (s t), s)
  | M.ASSIGN (e1, e2) ->
      let t1, s1 = infer tyenv e1 in
      let t2, s2 = infer (subst_env s1 tyenv) e2 in
      let s3 = unify (s2 t1) (TLoc t2) in
      ((s3 @@ s2 @@ s1) t2, s3 @@ s2 @@ s1)
  | M.BANG e ->
      let t, s = infer tyenv e in
      let tv = TVar (new_var ()) in
      let s' = unify t (TLoc tv) in
      ((s' @@ s) tv, s' @@ s)
  | SEQ (e1, e2) ->
      let _, s1 = infer tyenv e1 in
      let t2, s2 = infer (subst_env s1 tyenv) e2 in
      (t2, s2 @@ s1)
  | PAIR (e1, e2) ->
      let t1, s1 = infer tyenv e1 in
      let t2, s2 = infer (subst_env s1 tyenv) e2 in
      (TPair ((s2 @@ s1) t1, (s2 @@ s1) t2), s2 @@ s1)
  | FST e ->
      let t, s = infer tyenv e in
      let tv1 = TVar (new_var ()) in
      let tv2 = TVar (new_var ()) in
      let s' = unify t (TPair (tv1, tv2)) in
      ((s' @@ s) tv1, s' @@ s)
  | SND e ->
      let t, s = infer tyenv e in
      let tv1 = TVar (new_var ()) in
      let tv2 = TVar (new_var ()) in
      let s' = unify t (TPair (tv1, tv2)) in
      ((s' @@ s) tv2, s' @@ s)

let rec convert t =
  match t with
  | TInt -> M.TyInt
  | TBool -> M.TyBool
  | TString -> M.TyString
  | TPair (t1, t2) -> M.TyPair (convert t1, convert t2)
  | TLoc t1 -> M.TyLoc (convert t1)
  | TFun _ -> raise (M.TypeError "no function type as result in M")
  | TVar v -> raise (M.TypeError "Unresolved type var")
  | TUnion _ -> raise (M.TypeError "Union type cannot appear as final type")

let check (exp : M.exp) : M.typ =
  let tyenv = [] in
  let typ, subst = infer tyenv exp in
  let final_typ = subst typ in
  print_endline (string_of_typ final_typ);
  convert final_typ
