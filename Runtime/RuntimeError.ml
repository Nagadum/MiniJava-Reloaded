type t =
  | Illegal_downcast of string * string
  | Invalid_reference

exception RuntimeError of t;;

(* Les erreurs. *)
let report_error = function
  | Illegal_downcast(c1,c2) ->
    print_string "Illegal downcast Error : ";
    print_endline (c1 ^ " is not a subtype of " ^ c2)
  | Invalid_reference ->
    print_string "Invalid reference Error : ";
    print_endline ("maybe you forgot to initialize an attribut")

let illegal_downcast c1 c2 =
  raise (RuntimeError(Illegal_downcast (c1, c2)))

let invalid_reference () =
  raise (RuntimeError(Invalid_reference))
