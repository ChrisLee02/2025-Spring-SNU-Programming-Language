(* 지도를 손으로 직접 그려보기 *)
type treasure = StarBox | NameBox of string
type key = Bar | Node of key * key
type map = End of treasure
| Branch of map * map
| Guide of string * map


exception IMPOSSIBLE 
let getReady: map -> key list = fun map -> []


