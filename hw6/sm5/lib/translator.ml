(*
 * SNU 4190.310 Programming Languages 2025 Spring
 * Homework "SM5"
 *)
(* TODO : complete this function *)
let rec trans : K.program -> Machine.command =
 fun k ->
  let push_unit = Machine.PUSH (Machine.Val Machine.Unit) in
  let unbind_and_clear = [ Machine.UNBIND; Machine.POP ] in
  (* literal values *)
  match k with
  | K.NUM i -> [ Machine.PUSH (Machine.Val (Machine.Z i)) ]
  | K.TRUE -> [ Machine.PUSH (Machine.Val (Machine.B true)) ]
  | K.FALSE -> [ Machine.PUSH (Machine.Val (Machine.B false)) ]
  | K.UNIT -> [ push_unit ]
  (* operation *)
  | K.ADD (e1, e2) -> trans e1 @ trans e2 @ [ Machine.ADD ]
  | K.SUB (e1, e2) -> trans e1 @ trans e2 @ [ Machine.SUB ]
  | K.MUL (e1, e2) -> trans e1 @ trans e2 @ [ Machine.MUL ]
  | K.DIV (e1, e2) -> trans e1 @ trans e2 @ [ Machine.DIV ]
  | K.EQUAL (e1, e2) -> trans e1 @ trans e2 @ [ Machine.EQ ]
  | K.LESS (e1, e2) -> trans e1 @ trans e2 @ [ Machine.LESS ]
  | K.NOT e -> trans e @ [ Machine.NOT ]
  (* variable and assign *)
  | K.VAR id -> [ Machine.PUSH (Machine.Id id); Machine.LOAD ]
  | K.ASSIGN (x, e) ->
      trans e
      @ [
          Machine.PUSH (Machine.Id x);
          Machine.STORE;
          Machine.PUSH (Machine.Id x);
          Machine.LOAD;
        ]
  (* let binding *)
  | K.LETV (x, e1, e2) ->
      trans e1
      @ [
          Machine.MALLOC;
          Machine.BIND x;
          Machine.PUSH (Machine.Id x);
          Machine.STORE;
        ]
      @ trans e2 @ unbind_and_clear
  (* control flow *)
  | K.IF (e1, e2, e3) -> trans e1 @ [ Machine.JTR (trans e2, trans e3) ]
  | K.SEQ (e1, e2) -> trans e1 @ [ Machine.POP ] @ trans e2
  | K.WHILE (e1, e2) ->
      (* self-recursion을 가능하도록 수정해야됨,,, 근데 씨발 어케함? *)
      (* 자자 딱 알려드립니다.. 함수 정보를 call 전에 PUSH하시면 됩니다.. *)
      (* 함수 바디 앞에 bind를 하는게 Env.bind fEnv_args_binded f (Proc (args, fBody, fEnv))에 해당. *)
      (* CaLL 전에 스택 위에다가 함수 정보를 하나 딱 깔아놓고, 호출될 함수 바디의 맨 앞에 Bind를 넣으면 됨. *)
      (*
           let rec loop = fun () ->
             if e1 then (
               e2;
               loop()
             ) else ()
           in
           loop()       
       *)
      let stack_and_call =
        [
          Machine.PUSH (Machine.Id "#while");
          Machine.PUSH (Machine.Id "#while");
          push_unit;
          Machine.MALLOC;
          Machine.CALL;
        ]
      in

      let while_body =
        trans e1 @ [ Machine.JTR (trans e2 @ stack_and_call, [ push_unit ]) ]
      in
      [
        Machine.PUSH (Machine.Fn ("#_", [ Machine.BIND "#while" ] @ while_body));
        Machine.BIND "#while";
      ]
      @ stack_and_call @ unbind_and_clear
  | K.FOR (x, e1, e2, e3) ->
      (* 
       let n1 = eval(e1) in
       let n2 = eval(e2) in
       if n1 > n2 then ()
       else
         let rec loop i =
           if i > n2 then ()
           else
             let x = i in
             eval(e3);
             loop (i + 1)
         in
         loop n1
     *)
      (* 기존 변수가 있다고 생각하고 짜야 함. 다시 말해 call by ref임.
         근데? 루프 후에는 x값이 원래 값의 +1로 원상복귀해야됨.
     *)
      let bind_id =
       fun (id, e) ->
        trans e
        @ [
            Machine.MALLOC;
            Machine.BIND id;
            Machine.PUSH (Machine.Id id);
            Machine.STORE;
          ]
      in

      let call_for_body =
       fun push_val ->
        [ Machine.PUSH (Machine.Id "#for"); Machine.PUSH (Machine.Id "#for") ]
        @ push_val
        @ [ Machine.MALLOC; Machine.CALL ]
      in

      let push_i_plus_1 =
        [
          Machine.PUSH (Machine.Id "#i");
          Machine.LOAD;
          Machine.PUSH (Machine.Val (Machine.Z 1));
          Machine.ADD;
        ]
      in

      let for_body =
        [
          Machine.PUSH (Machine.Id "#i");
          Machine.LOAD;
          Machine.PUSH (Machine.Id x);
          Machine.STORE;
        ]
        @ trans e3
        @ [
            Machine.POP;
            Machine.PUSH (Machine.Id "#i");
            Machine.LOAD;
            Machine.PUSH (Machine.Id "#n2");
            Machine.LOAD;
            Machine.EQ;
          ]
        @ [ Machine.JTR ([ push_unit ], call_for_body push_i_plus_1) ]
      in

      let push_bind_for_body =
        [
          Machine.PUSH (Machine.Fn ("#i", [ Machine.BIND "#for" ] @ for_body));
          Machine.BIND "#for";
        ]
      in

      let for_logic =
        push_bind_for_body
        @ call_for_body [ Machine.PUSH (Machine.Id "#n1"); Machine.LOAD ]
        @ unbind_and_clear
      in

      bind_id ("#n1", e1)
      @ bind_id ("#n2", e2)
      @ [
          Machine.PUSH (Machine.Id "#n2");
          Machine.LOAD;
          Machine.PUSH (Machine.Id "#n1");
          Machine.LOAD;
          Machine.LESS;
          Machine.JTR ([ push_unit ], for_logic);
        ]
      @ unbind_and_clear @ unbind_and_clear
  (* function *)
  | K.LETF (f, x, e1, e2) ->
      [
        Machine.PUSH (Machine.Fn (x, [ Machine.BIND f ] @ trans e1));
        Machine.BIND f;
      ]
      @ trans e2 @ unbind_and_clear
  | K.CALLV (f, e) ->
      [ Machine.PUSH (Machine.Id f); Machine.PUSH (Machine.Id f) ]
      @ trans e
      @ [ Machine.MALLOC; Machine.CALL ]
  | K.CALLR (f, x) ->
      [ Machine.PUSH (Machine.Id f); Machine.PUSH (Machine.Id f) ]
      @ [ Machine.PUSH (Machine.Id x); Machine.LOAD ]
      @ [ Machine.PUSH (Machine.Id x); Machine.CALL ]
  (* IO *)
  | K.READ x ->
      [
        Machine.GET;
        Machine.PUSH (Machine.Id x);
        Machine.STORE;
        Machine.PUSH (Machine.Id x);
        Machine.LOAD;
      ]
  | K.WRITE e ->
      trans e
      @ [
          (* save to temporal var *)
          Machine.MALLOC;
          Machine.BIND "#tmp";
          Machine.PUSH (Machine.Id "#tmp");
          Machine.STORE;
          (* write *)
          Machine.PUSH (Machine.Id "#tmp");
          Machine.LOAD;
          Machine.PUT;
          (* recover value to stack *)
          Machine.PUSH (Machine.Id "#tmp");
          Machine.LOAD;
          (* clean up *)
          Machine.UNBIND;
          Machine.POP;
        ]
