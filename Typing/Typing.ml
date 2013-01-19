open AST
open TypeError
open Env
open Type

let rec check_types type_asts env = 
  match type_asts with
    | [] -> env
    | c1::others -> try check_types others (addClass env c1.cname)
      with ClassAlreadyPresent _ -> type_clash c1.cname c1.cloc

let rec isSubtypeOf env t_p t_c =
  if (t_p = t_c)
  then true
  else 
    begin
    if (t_c = (fromString "Object"))
    then false
    else
      let t_cs = (findClass env (stringOf t_c)) in
      isSubtypeOf env t_p (getSuper t_cs)
    end

let rec check_expr_list loc lex lt env =
  match (lex, lt) with
    | ([], []) -> ()
    | ([], l) -> not_enough_args loc
    | (l, []) -> too_much_args loc
    | (e::l1, t::l2) -> 
      check_expr e env;
      match e.etype with
	| Some te ->
	  if ( isSubtypeOf env t te )
	  then check_expr_list loc l1 l2 env
	  else not_subtype (stringOf te) (stringOf t) loc
	| None -> 
	  if ( isSubtypeOf env t (fromString "None") )
	  then check_expr_list loc l1 l2 env
	  else not_subtype "None" (stringOf t) loc

and check_expr e env =
  match e.edesc with 
    | New s -> 
      if (not (isClass env s)) then unknown_type s e.eloc;
      e.etype <- Some (fromString s)
    | Seq (e1,e2) -> 
      check_expr e1 env;
      check_expr e2 env;
      e.etype <- e2.etype
    | Call (e0,fname,args) -> 
      check_expr e0 env; (*TODO*)
      begin match e0.etype with 
	| Some t -> 
	  begin 
	    try
	      let fargs = findFun env (stringOf t) fname in
	      check_expr_list e.eloc args fargs env
	    with Not_found -> unknown_meth fname (stringOf t) e.eloc
	  end
	| None -> unknown_meth fname "None" e.eloc
      end
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
		if (not (isSubtypeOf env var_type expr_type)) 
		then not_subtype (stringOf expr_type) (stringOf var_type) e.eloc
	      | None ->
		let expr_type = (fromString "None") in
		if (not (isSubtypeOf env var_type expr_type)) 
		then not_subtype (stringOf expr_type) (stringOf var_type) e.eloc
	    end
	with Not_found -> unknown_var s e.eloc
      end
    | Define (var_name,var_type,e0,e1) ->
      if (not(isClass env var_type)) then unknown_type var_type e.eloc;
      check_expr e0 env;
      begin match e0.etype with
	| Some expr_type ->
	  if (not (isSubtypeOf env (fromString var_type) expr_type)) 
	  then not_subtype (stringOf expr_type) var_type e.eloc
	| None ->
	  let expr_type = (fromString "None") in
	  if (not (isSubtypeOf env (fromString var_type) expr_type)) 
	  then not_subtype (stringOf expr_type) var_type e.eloc
      end;
      (* New variable with the same name hide the old ones *)
      let new_env = addVar env var_name (fromString var_type) in
      check_expr e1 new_env;
      e.etype <- e1.etype
    | Cast (t,e0) -> 
      if (not(isClass env t)) then unknown_type t e.eloc;
      let new_t = (fromString t) in
      check_expr e0 env;
      begin match e0.etype with
	| Some expr_type -> 
	  if ( ( isSubtypeOf env new_t expr_type) || ( isSubtypeOf env expr_type new_t ) )
	  then e.etype <- Some new_t
	  else not_castable (stringOf expr_type) (stringOf new_t) e.eloc
	| None -> 
	  let expr_type = (fromString "None") in
	  if ( ( isSubtypeOf env new_t expr_type) || ( isSubtypeOf env expr_type new_t ) )
	  then e.etype <- Some new_t
	  else not_castable (stringOf expr_type) (stringOf new_t) e.eloc
      end
    | Instanceof (e0,t) -> 
      if (not(isClass env t)) then unknown_type t e.eloc;
      check_expr e0 env;
      e.etype <- Some (fromString "Boolean")

let type_program (cl,eo) =
  let new_env = check_types cl (Env.initialEnv()) in
  match eo with
    | Some e -> check_expr e new_env;
    | None -> ()
