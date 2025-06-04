(*
 * SNU 4190.310 Programming Languages 
 * Homework "RozettaX" Skeleton
 *)

(* push (x, C′)의 결과가 (x, C′, E) :: S니까, 이 방식으로
호출 시점의 Env를를 저장해둬야 할 듯,, *)

(* Fn의 경우 맨 뒤에 ret 클로저를 id로 가져오고, 호출하는걸 붙여줘야 됨. *)
(* Call은 실제로 Fn을 Call하기 전에, 기존 ret 주소를 백업 후 
 기존 주소 복원하는 로직 + 뒷 cmds를 묶어서 새로운 ret 주소를 생성하여 저장
*)
(* JTR은 별거없음. 그냥 뒤에 cmds 붙여서 통번역 시키면 됨,, *)
(* 
   고로 첫 시작때 []을 가장 마지막 ret 클로저로 저장해두고 시작하면 된다..
*)
(* 
   Fn의 변환은 단순해야됨. 
   그냥 cont_id로 ret 클로저 가져와서 호출하는걸 뒤에 붙이는 것으로 마무리
*)
(* 
   기존 주소의 백업, 복원 로직을 포함한 ret 클로저 생성은 모두 Call에서 처리한다.
   스택으로 깔아놓고 넘겨줘야 함.. 그냥 PUSH하면 CALL에서 환경 바뀌기떄문에 인식못함.
*)

let cont_id = "#K"
let tmp_bind_id = "#tmp_bind"
let tmp_val_id = "#tmp_val"

let ret_code =
  [
    Sm5.PUSH (Sm5.Id cont_id);
    Sm5.PUSH (Sm5.Val Sm5.Unit);
    Sm5.PUSH (Sm5.Id tmp_val_id);
    Sm5.CALL;
  ]

let trans_v : Sm5.value -> Sonata.value = function
  | Sm5.Z z -> Sonata.Z z
  | Sm5.B b -> Sonata.B b
  | Sm5.L _ -> raise (Sonata.Error "Invalid input program : pushing location")
  | Sm5.Unit -> Sonata.Unit
  | Sm5.R _ -> raise (Sonata.Error "Invalid input program : pushing record")

(* TODO : complete this function *)
let rec trans_obj : Sm5.obj -> Sonata.obj = function
  | Sm5.Val v -> Sonata.Val (trans_v v)
  | Sm5.Id id -> Sonata.Id id
  | Sm5.Fn (arg, command) ->
      Sonata.Fn (arg, [ Sonata.BIND cont_id ] @ trans' (command @ ret_code))

(* TODO : complete this function *)
and trans' : Sm5.command -> Sonata.command = function
  | [] -> []
  | Sm5.PUSH obj :: cmds -> Sonata.PUSH (trans_obj obj) :: trans' cmds
  | Sm5.POP :: cmds -> Sonata.POP :: trans' cmds
  | Sm5.STORE :: cmds -> Sonata.STORE :: trans' cmds
  | Sm5.LOAD :: cmds -> Sonata.LOAD :: trans' cmds
  | Sm5.MALLOC :: cmds -> Sonata.MALLOC :: trans' cmds
  | Sm5.BOX z :: cmds -> Sonata.BOX z :: trans' cmds
  | Sm5.UNBOX id :: cmds -> Sonata.UNBOX id :: trans' cmds
  | Sm5.BIND id :: cmds -> Sonata.BIND id :: trans' cmds
  | Sm5.UNBIND :: cmds -> Sonata.UNBIND :: trans' cmds
  | Sm5.GET :: cmds -> Sonata.GET :: trans' cmds
  | Sm5.PUT :: cmds -> Sonata.PUT :: trans' cmds
  | Sm5.ADD :: cmds -> Sonata.ADD :: trans' cmds
  | Sm5.SUB :: cmds -> Sonata.SUB :: trans' cmds
  | Sm5.MUL :: cmds -> Sonata.MUL :: trans' cmds
  | Sm5.DIV :: cmds -> Sonata.DIV :: trans' cmds
  | Sm5.EQ :: cmds -> Sonata.EQ :: trans' cmds
  | Sm5.LESS :: cmds -> Sonata.LESS :: trans' cmds
  | Sm5.NOT :: cmds -> Sonata.NOT :: trans' cmds
  | Sm5.JTR (c1, c2) :: cmds ->
      [ Sonata.JTR (trans' (c1 @ cmds), trans' (c2 @ cmds)) ]
  | Sm5.CALL :: [] -> [ Sonata.CALL ]
  | Sm5.CALL :: cmds ->
      [
        Sonata.BIND tmp_bind_id;
        Sonata.PUSH (Sonata.Id tmp_val_id);
        Sonata.STORE;
        Sonata.BIND tmp_bind_id;
      ]
      @ [
          Sonata.PUSH (Sonata.Fn ("_", trans' cmds));
          Sonata.PUSH (Sonata.Id tmp_bind_id);
          Sonata.UNBIND;
          Sonata.POP;
          Sonata.PUSH (Sonata.Id tmp_val_id);
          Sonata.LOAD;
          Sonata.PUSH (Sonata.Id tmp_bind_id);
          Sonata.UNBIND;
          Sonata.POP;
          Sonata.CALL;
        ]

(* TODO : complete this function *)
let trans (command : Sm5.command) : Sonata.command =
  [ Sonata.MALLOC; Sonata.BIND tmp_val_id ] @ trans' command
