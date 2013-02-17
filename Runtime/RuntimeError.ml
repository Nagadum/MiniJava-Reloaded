type t =
  | Illegal_downcast of string * string

exception RuntimeError of t;;

(* Les erreurs. *)
let report_error = function
  | Illegal_downcast(c1,c2) ->
    print_string "Illegal downcast Error : ";
    print_endline (c1 ^ " is not a subtype of " ^ c2)

let illegal_downcast c1 c2 =
  raise (RuntimeError(Illegal_downcast (c1, c2)))

