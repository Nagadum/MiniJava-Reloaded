open AST
open TypeError

let rec find e l =
    match l with
        [] -> false
        |(h::t) -> if (h = e) then true else find e t

let type_program (cl,eo) =
  ()

let rec check_types type_asts types = 
  match type_asts with
    | [] -> types
    | c1::others -> if (find c1.cname types) then type_clash c1.cname c1.cloc;
	  check_types others (c1.cname::types)
	  
