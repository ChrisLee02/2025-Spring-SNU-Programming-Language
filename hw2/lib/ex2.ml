type metro =
  | STATION of name
  | AREA of name * metro
  | CONNECT of metro * metro

and name = string
and closure = name list

let checkMetro metro =
  let rec checkWithClosure metro closure =
    match metro with
    | STATION name -> List.mem name closure
    | AREA (name, metro) ->
        checkWithClosure metro (name :: closure)
    | CONNECT (metro1, metro2) ->
        checkWithClosure metro1 closure
        && checkWithClosure metro2 closure
  in

  checkWithClosure metro []
