(*
 * SNU 4190.310 Programming Languages 
 * Homework "Continuation Passing Style" Skeleton
 *)

open M0

let count = ref 0

let new_name () =
  let _ = count := !count + 1 in
  "x_" ^ string_of_int !count

let rec alpha_conv exp subst =
  match exp with
  | Num n -> Num n
  | Var x -> ( try Var (List.assoc x subst) with Not_found -> Var x)
  | Fn (x, e) ->
      let x' = new_name () in
      let subst' = (x, x') :: subst in
      Fn (x', alpha_conv e subst')
  | Fnr (f, x, e) ->
      let x' = new_name () in
      let f' = new_name () in
      let subst' = (f, f') :: (x, x') :: subst in
      Fnr (f', x', alpha_conv e subst')
  | App (e1, e2) -> App (alpha_conv e1 subst, alpha_conv e2 subst)
  | Ifp (e1, e2, e3) ->
      Ifp (alpha_conv e1 subst, alpha_conv e2 subst, alpha_conv e3 subst)
  | Add (e1, e2) -> Add (alpha_conv e1 subst, alpha_conv e2 subst)
  | Pair (e1, e2) -> Pair (alpha_conv e1 subst, alpha_conv e2 subst)
  | Fst e -> Fst (alpha_conv e subst)
  | Snd e -> Snd (alpha_conv e subst)

(* TODO : Complete this function *)
let rec cps' exp =
  let k = new_name () in
  match exp with
  (* Constant expressions *)
  | Num n -> Fn (k, App (Var k, Num n)) (* simply k(n) *)
  | Var x -> Fn (k, App (Var k, Var x)) (* simply k(x) *)
  | Fn (x, e) ->
      (* 
        make function body as cps one
      *)
      Fn (k, App (Var k, Fn (x, cps' e)))
  | Fnr (f, x, e) -> Fn (k, App (Var k, Fnr (f, x, cps' e)))
  (* Non constant expressions *)
  | App (e1, e2) ->
      (* 
      let f1 = eval e1 in             
      let v2 = eval e2 in
      k(f1(v2))
      이를 위해서는 cps로 변환된 f1의 결과값에 k를 넘겨주면 된다. 
    *)
      let f1 = new_name () in
      let v2 = new_name () in
      (* *)
      Fn
        ( k,
          App
            ( cps' e1,
              Fn (f1, App (cps' e2, Fn (v2, App (App (Var f1, Var v2), Var k))))
            ) )
  | Ifp (e1, e2, e3) ->
      let v1 = new_name () in
      let v2 = new_name () in
      let v3 = new_name () in

      Fn
        ( k,
          App
            ( cps' e1,
              Fn
                ( v1,
                  Ifp
                    ( Var v1,
                      App (cps' e2, Fn (v2, App (Var k, Var v2))),
                      App (cps' e3, Fn (v3, App (Var k, Var v3))) ) ) ) )
  | Add (e1, e2) ->
      (* 
        let v1 = eval e1 in
        let v2 = eval e2
        in
        Fn (k, App (Var k, Add (v1, v2)))
        를 람다로 번역하는 것과 같다..!
      *)
      let v1 = new_name () in
      let v2 = new_name () in
      Fn
        ( k,
          App
            ( cps' e1,
              Fn (v1, App (cps' e2, Fn (v2, App (Var k, Add (Var v1, Var v2)))))
            ) )
  | Pair (e1, e2) ->
      let v1 = new_name () in
      let v2 = new_name () in
      (* App (Var k, Pair (v1, v2)) *)
      Fn
        ( k,
          App
            ( cps' e1,
              Fn (v1, App (cps' e2, Fn (v2, App (Var k, Pair (Var v1, Var v2)))))
            ) )
  | Fst e ->
      let v = new_name () in
      Fn (k, App (cps' e, Fn (v, App (Var k, Fst (Var v)))))
  | Snd e ->
      let v = new_name () in
      Fn (k, App (cps' e, Fn (v, App (Var k, Snd (Var v)))))

let cps exp = cps' (alpha_conv exp [])
