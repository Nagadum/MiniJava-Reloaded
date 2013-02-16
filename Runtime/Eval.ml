open AST
open Env
open TypeEnv
open String

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
    |Call(e1, "gt", e2::_) -> begin match((eval_expr e1 env), (eval_expr e2 env)) with
        | (Int a, Int b) -> Boolean ( a > b )
        | _ -> Null
    end
    |Call(e1, "ge", e2::_) -> begin match((eval_expr e1 env), (eval_expr e2 env)) with
        | (Int a, Int b) -> Boolean ( a >= b )
        | _ -> Null
    end
    |Call(e1, "lt", e2::_) -> begin match((eval_expr e1 env), (eval_expr e2 env)) with
        | (Int a, Int b) -> Boolean ( a < b )
        | _ -> Null
    end
    |Call(e1, "le", e2::_) -> begin match((eval_expr e1 env), (eval_expr e2 env)) with
        | (Int a, Int b) -> Boolean ( a <= b )
        | _ -> Null
    end 
    |Call(e1, "eq", e2::_) -> begin match((eval_expr e1 env), (eval_expr e2 env)) with
        | (Int a, Int b) -> Boolean ( a = b )
        | (Boolean a, Boolean b) -> Boolean ( a = b )
        | (String a, String b) -> Boolean ( (String.compare a b) = 0 )
        | (Null, Null) -> Boolean(true)
        | (Reference a, Reference b) -> Boolean ( a = b )
          (* TODO surcharge de == *)
        | _ -> Boolean(false) (*Deux choses incomparable ne sont jamais egales*)
    end
    |Call(e1, "neq", e2::_) -> begin match((eval_expr e1 env), (eval_expr e2 env)) with
        | (Int a, Int b) -> Boolean ( a <> b )
        | (Boolean a, Boolean b) -> Boolean ( a <> b )
        | (String a, String b) -> Boolean ( (String.compare a b) <> 0 )
        | (Null, Null) -> Boolean(true)
        | (Reference a, Reference b) -> Boolean ( a <> b )
          (* TODO surcharge de != *)
        | _ -> Boolean(true) (*Deux choses incomparable sont toujours differentes*)
    end
    | Call(e1, "neg", _) ->  begin match (eval_expr e1 env) with
	| Int a -> Int(-a)
	| _ -> Null
    end
    | New s -> Reference(TypeEnv.newObject env s)
    | Seq(e1, e2) -> begin match((eval_expr e1 env), (eval_expr e2 env)) with
        | (_, result) -> result
    end
    | If(cond, then_exp, None) -> 
      begin match((eval_expr cond env), (eval_expr then_exp env)) with
        | ( Boolean true, e0 ) -> e0
        | ( Boolean false, _ ) -> Null
        | _ -> Null
      end
    | If(cond, then_exp, Some else_exp) -> 
      begin match((eval_expr cond env), (eval_expr then_exp env), (eval_expr else_exp env)) with
        | ( Boolean true, e0, _ ) -> e0
        | ( Boolean false, _, e0) -> e0
        | _ -> Null
      end
    | Define (varname, vartype, varvalue, e) -> 
	let newenv = TypeEnv.addVar env varname (eval_expr varvalue env) in
	  eval_expr e newenv
    | Var v -> (findVar env)  v
    | Val v -> begin match v with 	
        | Int a -> Int(a)
        | Boolean a -> Boolean(a)
        | String a -> String(a)
        | Reference a -> Reference(a)
	| _ -> Null
    end
    | Assign(varname, varvalue) -> (* TODO *) Null
    | Cast(new_type, e) -> (* TODO *) Null
    | Instanceof(e, t) -> (* TODO *) Null
    | _ -> Null

