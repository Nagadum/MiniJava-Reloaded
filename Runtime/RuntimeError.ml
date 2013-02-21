type t =
  | Illegal_downcast of string * string
  | Invalid_reference
  | Internal_error of string

exception RuntimeError of t;;

(* Les erreurs. *)
let report_error = function
  | Illegal_downcast(c1,c2) ->
    print_string "Illegal downcast Error : ";
    print_endline (c1 ^ " is not a subtype of " ^ c2)
  | Invalid_reference ->
    print_string "Invalid reference Error : ";
    print_endline ("maybe you forgot to initialize an attribut")
  | Internal_error s->
    print_endline ("An unexpected error was encountered : " ^ s)

let illegal_downcast c1 c2 =
  raise (RuntimeError(Illegal_downcast (c1, c2)))

let invalid_reference () =
  raise (RuntimeError(Invalid_reference))

let internal_error s =
  raise (RuntimeError(Internal_error s))
