(* 지도를 손으로 직접 그려보기 *)
type treasure = StarBox | NameBox of string
type key = Bar | Node of key * key
type map = End of treasure
| Branch of map * map
| Guide of string * map

(* 지도가 암시하는 모양을 뽑아낸 후에, 거기서 최소크기로 줄이는게 필요할 듯? *)

exception IMPOSSIBLE 
let getReady: map -> key list = fun map -> []


