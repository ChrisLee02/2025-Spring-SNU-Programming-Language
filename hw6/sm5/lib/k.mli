(*
 * SNU 4190.310 Programming Languages 2025 Spring
 * K-* Interpreter
 *)

(** Location Signature *)
module Loc : sig
  type t

  val base : t
  val equal : t -> t -> bool
  val diff : t -> t -> int
  val increase : t -> int -> t
end

(** Memory Signature *)
module Mem : sig
  type 'a t
  type loc

  exception Not_allocated
  exception Not_initialized

  val empty : 'a t (* get empty memory *)
  val load : 'a t -> loc -> 'a (* load value : Mem.load mem loc => value *)

  val store :
    'a t -> loc -> 'a -> 'a t (* save value : Mem.store mem loc value => mem' *)

  val alloc :
    'a t ->
    loc * 'a t (* get fresh memory cell : Mem.alloc mem => (loc, mem') *)
end

(** Environment Signature *)
module Env : sig
  type ('a, 'b) t

  exception Not_bound

  val empty : ('a, 'b) t (* get empty environment *)
  val lookup : ('a, 'b) t -> 'a -> 'b
  (* lookup environment : Env.lookup env key => content *)

  val bind :
    ('a, 'b) t ->
    'a ->
    'b ->
    ('a, 'b) t (* id binding : Env.bind env key content => env'*)
end

exception Error of string

type id = string

type exp =
  | NUM of int
  | TRUE
  | FALSE
  | UNIT
  | VAR of id
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | EQUAL of exp * exp
  | LESS of exp * exp
  | NOT of exp
  | ASSIGN of id * exp (* assgin to variable *)
  | SEQ of exp * exp (* sequence *)
  | IF of exp * exp * exp (* if-then-else *)
  | WHILE of exp * exp (* while loop *)
  | FOR of id * exp * exp * exp (* for loop *)
  | LETV of id * exp * exp (* variable binding *)
  | LETF of id * id * exp * exp (* procedure binding *)
  | CALLV of id * exp (* call by value *)
  | CALLR of id * id (* call by referenece *)
  | READ of id
  | WRITE of exp

type program = exp
type memory
type env
type value

val emptyMemory : memory
val emptyEnv : env
val run : program -> value
