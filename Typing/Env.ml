open TypeError

exception ClassAlreadyPresent of string

type tEnv_v = (string, Type.t) Hashtbl.t 

and tEnv_c = (string, tClasse) Hashtbl.t 

and t_funs = (string, Type.t list) Hashtbl.t

and tEnv = {

  env_v : tEnv_v ;
  env_c : tEnv_c

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

let initialEnv () = 
  let result = makeEnv (Hashtbl.create 17 : tEnv_v) (Hashtbl.create 17 : tEnv_c) in
  Hashtbl.add result.env_c "Object" makeClass;
  Hashtbl.add result.env_c "None" makeClass;
  Hashtbl.add result.env_c "Int" makeClass;
  Hashtbl.add result.env_c "Boolean" makeClass;
  Hashtbl.add result.env_c "String" makeClass;
  result

let findVar env = Hashtbl.find (env.env_v)

let rec findInList l a = 
  match l with
    | [] -> false
    | (h::t) -> if (h = a) then true else findInList t a

let findClass env = Hashtbl.find (env.env_c) 

let isClass env c = 
  try findClass env c; true
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
