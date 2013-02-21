(* Environnement de typage *)

open TypeError

exception ClassAlreadyPresent of string
exception MethodAlreadyPresent of string

(* Environnement des variables *)
type tEnv_v = (string, Type.t) Hashtbl.t 

(* Environnement des classes *)
and tEnv_c = (string, tClasse) Hashtbl.t 

and t_funs = (string, tFun) Hashtbl.t

(* Environnement *)
and tEnv = {

  env_v : tEnv_v ;
  env_c : tEnv_c

}

and tArg = Type.t * string

(* Representation d'une fonction pour le typage *)
and tFun = {

  fargs : tArg list ;
  freturn : Type.t

}

(* Representation d'une classe pour le typage *)
and tClasse = {

  csuper : Type.t ;
  funs : t_funs

}

let makeClass = {
  csuper = (Type.fromString "Object"); 
  funs = (Hashtbl.create 17 : t_funs)
}

let getSuper c =
  c.csuper

let makeEnv v c = {
  env_v = v ;
  env_c = c 
}

let makeClassInt () =
  let c = makeClass in
  let t_int = (Type.fromString "Int") in
  let t_bool = (Type.fromString "Boolean") in
  Hashtbl.add c.funs "add" { fargs = [(t_int, "n")]; freturn = t_int };
  Hashtbl.add c.funs "sub" { fargs = [(t_int, "n")]; freturn = t_int };
  Hashtbl.add c.funs "mul" { fargs = [(t_int, "n")]; freturn = t_int };
  Hashtbl.add c.funs "div" { fargs = [(t_int, "n")]; freturn = t_int };
  Hashtbl.add c.funs "mod" { fargs = [(t_int, "n")]; freturn = t_int };
  Hashtbl.add c.funs "neg" { fargs = []; freturn = t_int };
  Hashtbl.add c.funs "gt"  { fargs = [(t_int, "n")]; freturn = t_bool };
  Hashtbl.add c.funs "ge"  { fargs = [(t_int, "n")]; freturn = t_bool };
  Hashtbl.add c.funs "lt"  { fargs = [(t_int, "n")]; freturn = t_bool };
  Hashtbl.add c.funs "le"  { fargs = [(t_int, "n")]; freturn = t_bool };
  c

let makeClassObject () =
  let c = makeClass in
  let t_obj = (Type.fromString "Object") in
  let t_bool = (Type.fromString "Boolean") in
  Hashtbl.add c.funs "eq"  { fargs = [(t_obj, "o")]; freturn = t_bool };
  Hashtbl.add c.funs "neq" { fargs = [(t_obj, "o")]; freturn = t_bool };
  c

let makeClassBool () =
  let c = makeClass in
  let t_bool = (Type.fromString "Boolean") in
  Hashtbl.add c.funs "and"  { fargs = [(t_bool, "b")]; freturn = t_bool };
  Hashtbl.add c.funs "or" { fargs = [(t_bool, "b")]; freturn = t_bool };
  Hashtbl.add c.funs "not" { fargs = []; freturn = t_bool };
  c

let initialEnv () = 
  let result = makeEnv (Hashtbl.create 17 : tEnv_v) (Hashtbl.create 17 : tEnv_c) in
  Hashtbl.add result.env_c "Object" (makeClassObject());
  Hashtbl.add result.env_c "Int" (makeClassInt());
  Hashtbl.add result.env_c "Boolean" (makeClassBool());
  Hashtbl.add result.env_c "String" makeClass;
  Hashtbl.add result.env_c "Null" makeClass ;
  result

(* Recherches dans l'environnement *)

let findVar env = Hashtbl.find (env.env_v)

let isVar env v =
  try findVar env v; true
  with Not_found -> false

let findClass env = Hashtbl.find (env.env_c) 

let isClass env c = 
  try findClass env c; true
  with Not_found -> false

let findFun env cname =
  Hashtbl.find (Hashtbl.find(env.env_c) cname).funs
  
(* Recherche une methode d'une classe, et si on ne la trouve pas,
continue de la chercher dans le parent de la classe *)
let rec findFun_rec env cname f =
  try
    Hashtbl.find (Hashtbl.find(env.env_c) cname).funs f
  with Not_found ->
    if (cname = "Object")
    then raise Not_found
    else 
      begin
        let c = findClass env cname in
        findFun_rec env (Type.stringOf c.csuper) f
      end

let isFun env c f =
  try findFun env c f; true
  with Not_found -> false

let isFun_rec env c f =
  try findFun_rec env c f; true
  with Not_found -> false

(* Ajouts à l'environnement *)

let addVar env n t = 
  let new_v = Hashtbl.copy env.env_v in
  Hashtbl.add new_v n t; 
  makeEnv new_v env.env_c

let addClass env c =
  if (isClass env c) then raise (ClassAlreadyPresent(c));
  let new_c = Hashtbl.copy env.env_c in
  Hashtbl.add new_c c makeClass ; 
  makeEnv env.env_v new_c

let addFun env cname fname f = 
  if (isFun env cname fname) then raise (MethodAlreadyPresent(fname)) ;
  let new_env_c = Hashtbl.copy env.env_c in
  let old_c = findClass env cname in
  let new_funs = Hashtbl.copy old_c.funs in
  Hashtbl.add new_funs fname f ;
  Hashtbl.add new_env_c cname {csuper = old_c.csuper; funs = new_funs} ;
  makeEnv env.env_v new_env_c
    
let setSuper env classname supername =
  let new_env_c = Hashtbl.copy env.env_c in
  let oldc = findClass env classname in
  Hashtbl.add new_env_c classname {csuper = (Type.fromString supername); funs = oldc.funs} ; 
  makeEnv env.env_v new_env_c
