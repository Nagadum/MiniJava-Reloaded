open AST
open TypeError
open Env
open Type

let rec check_types type_asts env = 
  match type_asts with
    | [] -> env
    | c1::others -> try check_types others (addClass env c1.cname)
      with ClassAlreadyPresent _ -> type_clash c1.cname c1.cloc

let rec isSubtypeOf t_p t_c =
  false (* TODO *)

let rec check_expr e env =
  match e.edesc with 
    | New s -> 
      if (not (findClass env s)) then unknown_type s e.eloc;
      e.etype <- Some (fromString s)
    | Seq (e1,e2) -> 
      check_expr e1 env;
      check_expr e2 env;
      e.etype <- e2.etype
    | Call (e,fname,args) -> (* TODO *) ()
    | If (e0, e1, e2) ->
      check_expr e0 env;
      begin match e0.etype with 
	| Some t -> if ( (stringOf t) <> "Boolean") then incorrect_type "Boolean" (stringOf t) e.eloc;
	| _ -> incorrect_type "Boolean" "None" e.eloc;
      end;
      check_expr e1 env;
      begin match e2 with
	| Some e2_ -> check_expr e2_ env;
	| None ->  ()
      end;
      e.etype <- e1.etype
    | Val v -> 
      begin match v with
	| String s -> e.etype <- Some (fromString "String")
	| Int i -> e.etype <- Some (fromString "Int")
	| Boolean b -> e.etype <- Some (fromString "Boolean")
	| Null -> e.etype <- Some (fromString "Null")
      end
    | Var s -> 
      begin
      try e.etype <- Some (findVar env s)
      with Not_found -> unknown_var s e.eloc
      end
    | Assign (s,e0) -> 
      begin
	try let var_type = (findVar env s) in
	    check_expr e0 env;
	    begin match e0.etype with
	      | Some expr_type ->
		if (not (isSubtypeOf var_type expr_type)) 
		then not_subtype (stringOf expr_type) (stringOf var_type) e.eloc
	      | None ->
		let expr_type = (fromString "None") in
		if (not (isSubtypeOf var_type expr_type)) 
		then not_subtype (stringOf expr_type) (stringOf var_type) e.eloc
	    end
	with Not_found -> unknown_var s e.eloc
      end
    | Define (var_name,var_type,e0,e1) ->
      if (not(findClass env var_type)) then unknown_type var_type e.eloc;
      check_expr e0 env;
      begin match e0.etype with
	| Some expr_type ->
	  if (not (isSubtypeOf var_type expr_type)) 
	  then not_subtype (stringOf expr_type) var_type e.eloc
	| None ->
	  let expr_type = (fromString "None") in
	  if (not (isSubtypeOf var_type expr_type)) 
	  then not_subtype (stringOf expr_type) var_type e.eloc
      end;
      (* New variable with the same name hide the old ones *)
      let new_env = addVar env var_name (fromString var_type) in
      check_expr e1 new_env;
      e.etype <- e1.etype
    | Cast (t,e0) -> 
      if (not(findClass env t)) then unknown_type t e.eloc;
      let new_t = (fromString t) in
      check_expr e0 env;
      begin match e0.etype with
	| Some expr_type -> 
	  if ( ( isSubtypeOf new_t expr_type) || ( isSubtypeOf expr_type new_t ) )
	  then e.etype <- Some new_t
	  else not_castable (stringOf expr_type) (stringOf new_t) e.eloc
	| None -> 
	  let expr_type = (fromString "None") in
	  if ( ( isSubtypeOf new_t expr_type) || ( isSubtypeOf expr_type new_t ) )
	  then e.etype <- Some new_t
	  else not_castable (stringOf expr_type) (stringOf new_t) e.eloc
      end
    | Instanceof (e0,t) -> 
      if (not(findClass env t)) then unknown_type t e.eloc;
      check_expr e0 env;
      e.etype <- Some (fromString "Boolean")
    | _ -> ()

let type_program (cl,eo) =
  let new_env = check_types cl (Env.initialEnv()) in
  match eo with
    | Some e -> check_expr e new_env;
    | None -> ()
