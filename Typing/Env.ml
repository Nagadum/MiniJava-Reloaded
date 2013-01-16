open TypeError

exception ClassAlreadyPresent of string

type tEnv_v = (string, Type.t) Hashtbl.t 

and tEnv_c = string list

and tEnv = {

  env_v : tEnv_v ;
  env_c : tEnv_c

}

let makeEnv v c = {
  
  env_v = v ;
  env_c = c 

}

let initialEnv () = 
  makeEnv (Hashtbl.create 17 : tEnv_v) ["String";"Int";"Boolean"]

let findVar env = Hashtbl.find (env.env_v)

let rec findInList l a = 
  match l with
    | [] -> false
    | (h::t) -> if (h = a) then true else findInList t a

let findClass env = findInList (env.env_c) 

let addVar env n t = 
  let new_v = Hashtbl.copy env.env_v in
    Hashtbl.add new_v n t; 
    makeEnv new_v env.env_c

let addClass env c =
  if (findClass env c) then raise (ClassAlreadyPresent(c));
  makeEnv env.env_v (c::(env.env_c))
