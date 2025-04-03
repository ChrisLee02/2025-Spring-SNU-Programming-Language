type token =
  | UNIT
  | NUM of (
# 31 "parser.mly"
        int
# 7 "parser.mli"
)
  | TRUE
  | FALSE
  | ID of (
# 33 "parser.mly"
        string
# 14 "parser.mli"
)
  | PLUS
  | MINUS
  | STAR
  | SLASH
  | EQUAL
  | LB
  | RB
  | LBLOCK
  | RBLOCK
  | NOT
  | COLONEQ
  | SEMICOLON
  | COMMA
  | PERIOD
  | IF
  | THEN
  | ELSE
  | END
  | WHILE
  | DO
  | LET
  | IN
  | READ
  | WRITE
  | PROC
  | LP
  | RP
  | LC
  | RC
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> K.K.exp
