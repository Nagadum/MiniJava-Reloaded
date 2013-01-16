open AST
open TypeError
open Env

let rec check_types type_asts env = 
  match type_asts with
    | [] -> env
    | c1::others -> try check_types others (addClass env c1.cname)
      with ClassAlreadyPresent _ -> type_clash c1.cname c1.cloc

let type_program (cl,eo) =
  let new_env = check_types cl (Env.initialEnv()) in
  ()
