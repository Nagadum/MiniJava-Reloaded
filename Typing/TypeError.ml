type t =
  | Typeclash of string
  | Attributeclash of string
  | Methodclash of string
  | Unknowntype of string
  | Unknownmeth of string * string
  | Notsubtype of string * string
  | Notcastable of string * string
  | Inheritancecycle of string * string
  | Toomuchargs
  | Notenoughargs
  | IncorrectType of string * string
  | Unknownvar of string
(* Lorsque le typeur rencontre une erreur inatendue *)
  | Typingerror

exception Error of t * Location.t

(* Les erreurs. *)
let report_error = function
  | Typeclash t ->
      print_string ("Type already defined "^t^": ")
  | Attributeclash a ->
      print_string ("Attribute already defined "^a^": ")
  | Methodclash m ->
      print_string ("Method already defined "^m^": ")
  | Unknowntype t ->
      print_string ("Unknown type "^t^": ")
  | Unknownmeth(s,t) ->
      print_string ("Unknown method "^s^" in "^t^": ")
  | Notsubtype(t1,t2) ->
      print_string (t1^" is not subtype of "^t2^": ")
  | Notcastable(t1,t2) ->
      print_string (t1^" is not castable to "^t2^": ")
  | Inheritancecycle(t1,t2) ->
      print_string (t2^" is already subtype of "^t1^": ")
  | Toomuchargs -> 
      print_string "Too much arguments: "
  | Notenoughargs ->
      print_string "Not enough arguments: "
  | IncorrectType(t1,t2) ->
      print_string ("Expected " ^ t1 ^ " ,found " ^ t2 ^": ")
  | Unknownvar a ->
      print_string ("Unknown variable " ^ a)
  | Typingerror ->
      print_string ("An error occured while typing")

let not_subtype t1 t2 loc =
  raise (Error(Notsubtype(t1,t2),loc))

let not_castable t1 t2 loc =
  raise (Error(Notcastable(t1,t2),loc))

let inheritance_cycle t1 t2 loc =
  raise (Error(Inheritancecycle(t1,t2),loc))

let unknown_type t loc =
  raise (Error(Unknowntype t,loc))

let type_clash t loc =
  raise (Error(Typeclash t,loc))

let attribute_clash a loc =
  raise (Error(Attributeclash a,loc))

let method_clash m loc =
  raise (Error(Methodclash m,loc))

let unknown_meth m t loc =
  raise (Error(Unknownmeth(m,t),loc))

let too_much_args loc =
  raise (Error(Toomuchargs, loc))

let not_enough_args loc =
  raise (Error(Notenoughargs, loc))

let incorrect_type t1 t2 loc =
  raise (Error(IncorrectType(t1,t2), loc))

let unknown_var a loc =
  raise (Error(Unknownvar a,loc))

let typing_error loc =
  raise (Error(Typingerror, loc))

