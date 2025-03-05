type team =
  | Korea
  | France
  | Usa
  | Brazil
  | Japan
  | Nigeria
  | Cameroon
  | Poland
  | Portugal
  | Italy
  | Germany
  | Norway
  | Sweden
  | England
  | Argentina

type tourna = LEAF of team | NODE of tourna * tourna

let team_to_string : team -> string =
 fun t ->
  match t with
  | Korea -> "Korea"
  | France -> "France"
  | Usa -> "Usa"
  | Brazil -> "Brazil"
  | Japan -> "Japan"
  | Nigeria -> "Nigeria"
  | Cameroon -> "Cameroon"
  | Poland -> "Poland"
  | Portugal -> "Portugal"
  | Italy -> "Italy"
  | Germany -> "Germany"
  | Norway -> "Norway"
  | Sweden -> "Sweden"
  | England -> "England"
  | Argentina -> "Argentina"

let rec parenize : tourna -> string =
 fun tourna ->
  match tourna with
  | NODE (tourna1, tourna2) ->
      Printf.sprintf "(%s %s)" (parenize tourna1) (parenize tourna2)
  | LEAF team -> team_to_string team
