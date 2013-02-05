open TypeError

exception ClassAlreadyPresent of string
exception MethodAlreadyPresent of string

type tEnv_v = (string, Type.t) Hashtbl.t 

and tEnv_c = (string, tClasse) Hashtbl.t 

and t_funs = (string, tFun) Hashtbl.t

and tEnv = {

  env_v : tEnv_v ;
  env_c : tEnv_c

}

and tArg = Type.t * string

and tFun = {

  fargs : tArg list ;
  freturn : Type.t

}

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
  Hashtbl.add c.funs "add" { fargs = [(t_int, "n")]; freturn = t_int };
  Hashtbl.add c.funs "sub" { fargs = [(t_int, "n")]; freturn = t_int };
  Hashtbl.add c.funs "mul" { fargs = [(t_int, "n")]; freturn = t_int };
  Hashtbl.add c.funs "div" { fargs = [(t_int, "n")]; freturn = t_int };
  Hashtbl.add c.funs "mod" { fargs = [(t_int, "n")]; freturn = t_int };
  c

let initialEnv () = 
  let result = makeEnv (Hashtbl.create 17 : tEnv_v) (Hashtbl.create 17 : tEnv_c) in
  Hashtbl.add result.env_c "Object" makeClass;
  Hashtbl.add result.env_c "None" makeClass;
  Hashtbl.add result.env_c "Int" (makeClassInt());
  Hashtbl.add result.env_c "Boolean" makeClass;
  Hashtbl.add result.env_c "String" makeClass;
  result

let findVar env = Hashtbl.find (env.env_v)

let isVar env v =
  try findVar env v; true
  with Not_found -> false

let rec findInList l a = 
  match l with
    | [] -> false
    | (h::t) -> if (h = a) then true else findInList t a

let findClass env = Hashtbl.find (env.env_c) 

let isClass env c = 
  try findClass env c; true
  with Not_found -> false

let findFun env c =
  Hashtbl.find (Hashtbl.find(env.env_c) c).funs

let isFun env c f =
  try findFun env c f; true
  with Not_found -> false

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
