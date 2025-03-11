type ae =
  | CONST of int
  | VAR of string
  | POWER of string * int
  | TIMES of ae list
  | SUM of ae list

exception InvalidArgument

let rec diff : ae * string -> ae =
 fun (ae, str) ->
  let rec remove_ith n lst =
    match lst with
    | [] -> []
    | hd :: tl ->
        if n = 0 then tl else hd :: remove_ith (n - 1) tl
  in
  match ae with
  | CONST _ -> CONST 0
  | VAR s -> if s = str then CONST 1 else CONST 0
  | POWER (s, n) ->
      if s = str then TIMES [ CONST n; POWER (s, n - 1) ]
      else CONST 0
  | TIMES lst ->
      if List.is_empty lst then raise InvalidArgument
      else
        SUM
          (List.mapi
             (fun i x ->
               TIMES (diff (x, str) :: remove_ith i lst))
             lst)
  | SUM lst ->
      if List.is_empty lst then raise InvalidArgument
      else SUM (List.map (fun x -> diff (x, str)) lst)
