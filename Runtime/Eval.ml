(* Execution d'un programme minijava *)

open AST
open Env
open TypeEnv
open String
open RuntimeError

let rec addArgsToEnv env args values =
  match (args, values) with
    | ([],[]) -> env
    | (arg::q1, value::q2) -> 
      let newEnv = TypeEnv.addVar env arg value in
      addArgsToEnv newEnv q1 q2
    | _ -> internal_error("Number of argument passed is incoherent with the method definition.")

(* Genere l'environnement d'appel de fonctions *)
and callEnv env p args values =
  let newEnv = TypeEnv.makeCallEnv env in
  let envWithAttrs = TypeEnv.addAttrsToEnv newEnv p in
  addArgsToEnv envWithAttrs args values

(* Evalue une liste d'expressions *)
and eval_expr_list l env =
  match l with
    | [] -> []
    | e1::others -> (eval_expr e1 env)::(eval_expr_list others env)
  
and eval_expr e env =
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
        | _ -> Boolean(false) (*Deux choses incomparable ne sont jamais egales*)
    end
    |Call(e1, "neq", e2::_) -> begin match((eval_expr e1 env), (eval_expr e2 env)) with
        | (Int a, Int b) -> Boolean ( a <> b )
        | (Boolean a, Boolean b) -> Boolean ( a <> b )
        | (String a, String b) -> Boolean ( (String.compare a b) <> 0 )
        | (Null, Null) -> Boolean(true)
        | (Reference a, Reference b) -> Boolean ( a <> b )
        | _ -> Boolean(true) (*Deux choses incomparable sont toujours differentes*)
    end
    | Call(e1, "neg", _) ->  begin match (eval_expr e1 env) with
	| Int a -> Int(-a)
	| _ -> Null
    end
(* Execution d'une fonction *)
    | Call (e1, fname, args) -> begin match (eval_expr e1 env) with
        | Reference (-1) -> RuntimeError.invalid_reference()
        | Reference a -> 
          let cname = getType env a in
          let f = TypeEnv.getFun env cname fname in
          let args_value = eval_expr_list args env in
          let newEnv = callEnv env a f.fargs args_value in
          let result = eval_expr f.fbody newEnv in
          env.next <- newEnv.next ;
          result
        | _ -> Null
    end

    | New s -> Reference(TypeEnv.newObject env (eval1 env) s)

    | Seq(e1, e2) -> (eval_expr e1 env); (eval_expr e2 env)

    | If(cond, then_exp, None) -> 
      begin match (eval_expr cond env) with
        | ( Boolean true) -> (eval_expr then_exp env)
        | ( Boolean false) -> Null
        | _ -> Null
      end

    | If(cond, then_exp, Some else_exp) -> 
      begin match (eval_expr cond env) with
        | Boolean true -> (eval_expr then_exp env)
        | Boolean false -> (eval_expr else_exp env)
        | _ -> Null
      end

    | Define (varname, vartype, varvalue, e) -> 
      let evalue = (eval_expr varvalue env) in
      let newenv = TypeEnv.addVar env varname evalue in
	  eval_expr e newenv

    | Var v -> (findVar env)  v

    | Val v -> begin match v with 	
        | Int a -> Int(a)
        | Boolean a -> Boolean(a)
        | String a -> String(a)
        | Reference a -> Reference(a)
	| Null -> Null
    end

(* Le resultat d'une assignation est Null *)
    | Assign(varname, varvalue) -> 
      TypeEnv.setAttr env varname (eval_expr varvalue env) ;
      Null

(* Si B extends A, on ne sait pas au typage si une variable de type A est en fait
B. Donc le cast peut échouer à l'execution
On n'autorise pas pour l'instant le cast des types de base.
*)
    | Cast(new_type, e) ->
      begin match (eval_expr e env) with
        | Int a -> Int(a)
        | Boolean a -> Boolean(a)
        | String a -> String(a)
        | Reference a -> 
          if (TypeEnv.isInstance env new_type a)
          then Reference(a)
          else RuntimeError.illegal_downcast (TypeEnv.getType env a) new_type
	| Null -> Null
      end

    | Instanceof(e, t) -> 
      begin match (eval_expr e env) with
        | Int a -> Boolean(t = "Int")
        | Boolean a -> Boolean(t = "Boolean")
        | String a -> Boolean(t = "String")
        | Reference a -> Boolean(TypeEnv.isInstance env t a)
	| _ -> Boolean(false)
      end

and eval1 env e =
  eval_expr e env

