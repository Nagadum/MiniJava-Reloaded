open AST
open Env
open TypeEnv

let rec eval_expr e env =
  match e.edesc with
    | Call(e1, "not", _) ->  begin match (eval_expr e1 env) with
	| Boolean a -> Boolean(not a)
	| _ -> Null
    end
    | Call(e1, "sub", e2::_) ->  begin match ((eval_expr e1 env), (eval_expr e2 env)) with
	| (Int a, Int b) -> Int(a-b)
	| _ -> Null
    end
    | Call(e1, "add", e2::_) ->  begin match ((eval_expr e1 env), (eval_expr e2 env)) with
	| (Int a, Int b) -> Int(a+b)
	| _ -> Null
    end
    | Call(e1, "mul", e2::_) ->  begin match ((eval_expr e1 env), (eval_expr e2 env)) with
	| (Int a, Int b) -> Int(a*b)
	| _ -> Null
    end
    | Call(e1, "div", e2::_) ->  begin match ((eval_expr e1 env), (eval_expr e2 env)) with
	| (Int a, Int b) -> Int(a/b)
	| _ -> Null
    end
    | Call(e1, "mod", e2::_) ->  begin match ((eval_expr e1 env), (eval_expr e2 env)) with
	| (Int a, Int b) -> Int(a mod b)
	| _ -> Null
    end
    | Call(e1, "and", e2::_) ->  begin match ((eval_expr e1 env), (eval_expr e2 env)) with
	| (Boolean a, Boolean b) -> Boolean (a && b)
	| _ -> Null
    end
    | Call(e1, "or", e2::_) ->  begin match ((eval_expr e1 env), (eval_expr e2 env)) with
	| (Boolean a, Boolean b) -> Boolean (a || b)
	| _ -> Null
    end
    | Define (varname, vartype, varvalue, e) -> 
	let newenv = TypeEnv.addVar env varname (eval_expr varvalue env) in
	  eval_expr e newenv
    | Var v -> (findVar env)  v
    | Val v -> begin match v with 	
	| Int a -> Int(a)
	| _ -> Null
      end
    | _ -> Null

