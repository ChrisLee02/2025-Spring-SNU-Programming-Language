(*
 * SNU 4190.310 Programming Languages 
 * Homework "RozettaX" Skeleton
 *)

(* 메모리 기반으로 접근
    malloc으로 base 메모리를 두고, call depth를 +해서 depth마다의 
    continuation을 관리한다

    cont_base_loc + call_depth의 cont는 그 depth에서 call하는 시점의 cont를 저장
    즉, depth에서 return 시에는 depth-1의 cont를 가져온다.

    call_depth가 0인 경우는 따로 처리해줘야 할듯?

    모든 cont에는 직전 call_depth에 접근해서 그걸 호출하는 재귀적인 정의가 포함되어야 함.


*)

let cont_base_loc = "#K_base"
let call_depth = "#call_depth"
let get_cont_base_loc = [ Sonata.PUSH (Sonata.Id cont_base_loc); Sonata.LOAD ]
let get_call_depth = [ Sonata.PUSH (Sonata.Id call_depth); Sonata.LOAD ]
let get_cont_loc = get_cont_base_loc @ get_call_depth @ [ Sonata.ADD ]

let decrement_call_depth =
  get_call_depth
  @ [
      Sonata.PUSH (Sonata.Val (Sonata.Z 1));
      Sonata.SUB;
      Sonata.PUSH (Sonata.Id call_depth);
      Sonata.STORE;
    ]

let increment_call_depth =
  get_call_depth
  @ [
      Sonata.PUSH (Sonata.Val (Sonata.Z 1));
      Sonata.ADD;
      Sonata.PUSH (Sonata.Id call_depth);
      Sonata.STORE;
    ]

(* 현재 call depth의 continuation을 가져와서, 이전 call depth의 continuation을
   가져온다. *)
let terminate =
  [
    Sonata.PUSH (Sonata.Fn ("_", []));
    Sonata.PUSH (Sonata.Val Sonata.Unit);
    Sonata.MALLOC;
    Sonata.CALL;
  ]

let call_prev_cont =
  decrement_call_depth @ get_cont_loc @ [ Sonata.LOAD ]
  @ [ Sonata.PUSH (Sonata.Val Sonata.Unit); Sonata.MALLOC; Sonata.CALL ]

(* 만약 CALL_DEPTH가 0이라면, 빈 배열 함수 하나 만들어서 Call하여 종료료 *)

(* TODO : fill in here *)

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
      let sonata_command = trans' command @ call_prev_cont in
      Sonata.Fn (arg, sonata_command)

(* TODO : complete this function *)
and trans' : Sm5.command -> Sonata.command = function
  | Sm5.PUSH obj :: cmds -> Sonata.PUSH (trans_obj obj) :: trans' cmds
  | Sm5.POP :: cmds -> Sonata.POP :: trans' cmds
  | Sm5.STORE :: cmds -> Sonata.STORE :: trans' cmds
  | Sm5.LOAD :: cmds -> Sonata.LOAD :: trans' cmds
  | Sm5.JTR (c1, c2) :: cmds ->
      [ Sonata.JTR (trans' (c1 @ cmds), trans' (c2 @ cmds)) ]
  | Sm5.MALLOC :: cmds -> Sonata.MALLOC :: trans' cmds
  | Sm5.BOX z :: cmds -> Sonata.BOX z :: trans' cmds
  | Sm5.UNBOX id :: cmds -> Sonata.UNBOX id :: trans' cmds
  | Sm5.BIND id :: cmds -> Sonata.BIND id :: trans' cmds
  | Sm5.UNBIND :: cmds -> Sonata.UNBIND :: trans' cmds
  | Sm5.GET :: cmds -> Sonata.GET :: trans' cmds
  | Sm5.PUT :: cmds -> Sonata.PUT :: trans' cmds
  | Sm5.CALL :: cmds ->
      let main_cont =
        Sonata.Fn ("_", [ Sonata.UNBIND ] @ [ Sonata.POP ] @ trans' cmds)
      in
      let cont_with_prev =
        Sonata.Fn
          ( "_",
            [ Sonata.UNBIND ] @ [ Sonata.POP ] @ trans' cmds @ call_prev_cont )
      in
      get_call_depth
      @ [ Sonata.PUSH (Sonata.Val (Sonata.Z 1)); Sonata.LESS ]
      @ [
          Sonata.JTR ([ Sonata.PUSH main_cont ], [ Sonata.PUSH cont_with_prev ]);
        ]
      @ get_cont_loc @ [ Sonata.STORE ] @ increment_call_depth @ [ Sonata.CALL ]
  | Sm5.ADD :: cmds -> Sonata.ADD :: trans' cmds
  | Sm5.SUB :: cmds -> Sonata.SUB :: trans' cmds
  | Sm5.MUL :: cmds -> Sonata.MUL :: trans' cmds
  | Sm5.DIV :: cmds -> Sonata.DIV :: trans' cmds
  | Sm5.EQ :: cmds -> Sonata.EQ :: trans' cmds
  | Sm5.LESS :: cmds -> Sonata.LESS :: trans' cmds
  | Sm5.NOT :: cmds -> Sonata.NOT :: trans' cmds
  | [] -> []

(* TODO : complete this function *)
let trans (command : Sm5.command) : Sonata.command =
  [
    Sonata.MALLOC;
    Sonata.BIND cont_base_loc;
    Sonata.MALLOC;
    Sonata.PUSH (Sonata.Id cont_base_loc);
    Sonata.STORE;
    Sonata.MALLOC;
    Sonata.BIND call_depth;
    Sonata.PUSH (Sonata.Val (Sonata.Z 0));
    Sonata.PUSH (Sonata.Id call_depth);
    Sonata.STORE;
    Sonata.PUSH (Sonata.Val (Sonata.Z 0));
    Sonata.PUSH (Sonata.Id cont_base_loc);
    Sonata.LOAD;
    Sonata.STORE;
  ]
  @ trans' command
