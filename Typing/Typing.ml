open AST
open TypeError
open Env
open Type

let rec check_types type_asts env = 
  match type_asts with
    | [] -> env
    | c1::others -> try check_types others (addClass env c1.cname)
      with ClassAlreadyPresent _ -> type_clash c1.cname c1.cloc

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
	| _ -> () (* Should never happen *)
      end;
      check_expr e1 env;
      begin match e2 with
	| Some e2_ -> check_expr e2_ env;
	| None ->  ()
      end;
      e.etype <- e1.etype
    | _ -> ()

let type_program (cl,eo) =
  let new_env = check_types cl (Env.initialEnv()) in
  match eo with
    | Some e ->  check_expr e new_env
    | None -> ()
