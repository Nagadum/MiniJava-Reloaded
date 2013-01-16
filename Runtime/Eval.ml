open AST
open Env
open TypeEnv

let rec eval_expr e env =
  match e.edesc with
    | Call(e1, "add", e2::_) ->  begin match ((eval_expr e1 env), (eval_expr e2 env)) with
	| (Int a, Int b) -> Int(a+b)
	| _ -> Null
      end
    | Define (varname, vartype, varvalue, e) -> 
	let newenv = TypeEnv.defineVar env varname (eval_expr varvalue env) in
	  eval_expr e newenv
    | Var v -> (findVar env)  v
    | Val v -> begin match v with 	
	| Int a -> Int(a)
	| _ -> Null
      end
    | _ -> Null

