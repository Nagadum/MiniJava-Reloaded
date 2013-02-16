(* Definition de l'environnement d'execution *)

(* Tas *)
type tEnv_t = (int, tObject) Hashtbl.t 

(* Espace des classe *)
and tEnv_c = (string, tClasse) Hashtbl.t

(* Espaces des variables locales *)
and tEnv_v = (string, AST.value) Hashtbl.t

(* Espaces des fonctions *)
and tEnv_f = (fun_id, tFunction) Hashtbl.t

and tEnv = {

    env_t : tEnv_t ;
    env_c : tEnv_c ;
    env_v : tEnv_v ;
    env_f : tEnv_f ;
    mutable next : int
}

and tObject = {

    myClass : string 

}

and tConst = unit -> tObject

and tClasse = {

    super : string ;
    const : tConst ;
    functions :  ( string * fun_id) list 

}

and fun_id = string

and tFunction = {

    dummy : string

}

(* Instanciations des types *)

let makeObject cname = {
  myClass = cname
}

let makeConst cname ()=
  makeObject cname


let makeClass cname supername = {
  super = supername;
  const = makeConst cname;
  functions = []
}


let makeEnv e_t e_c e_v e_f i = {
    env_t = e_t;
    env_c = e_c;
    env_v = e_v;
    env_f = e_f;
    next = i
}

(*Recherches dans l'environnement*)

let findObj env = Hashtbl.find (env.env_t)

let findClass env = Hashtbl.find (env.env_c)

let findVar env = Hashtbl.find (env.env_v)

let findFun env = Hashtbl.find (env.env_f)

(*Modifications de l'environnement*)

let initialEnv () = 
  makeEnv (Hashtbl.create 17 : tEnv_t) (Hashtbl.create 17 : tEnv_c) (Hashtbl.create 17 : tEnv_v) (Hashtbl.create 17 : tEnv_f) 0

let addObj env n t = 
  let new_t = Hashtbl.copy (env.env_t) in
    Hashtbl.add new_t n t; 
    makeEnv new_t env.env_c env.env_v env.env_f env.next

let addClass env n t = 
  let new_c = Hashtbl.copy (env.env_c) in
    Hashtbl.add new_c n t; 
    makeEnv env.env_t new_c env.env_v env.env_f env.next

let addVar env n t = 
  let new_v = Hashtbl.copy (env.env_v) in
    Hashtbl.add new_v n t; 
    makeEnv env.env_t env.env_c new_v env.env_f env.next

let addFun env n t = 
  let new_f = Hashtbl.copy (env.env_f) in
    Hashtbl.add new_f n t; 
    makeEnv env.env_t env.env_c env.env_v new_f env.next

(*Affichage de l'environnement pour debug*)

let printClass cname c =
  print_endline ("---\nClass : " ^ cname);
  print_endline ("extends " ^ c.super)

let printEnv env =
  Hashtbl.iter printClass env.env_c

(* Instantiations *)

let newObject env cname =
  let id = env.next in
  let c = findClass env cname in
  let obj = makeObject cname in
  Hashtbl.add env.env_t id obj;
  env.next <- (id + 1);
  id

(* Gestion du polymorphisme *)

let rec isSubtype env cc cp =
  if (cc = cp)
  then true
  else if (cc = "Object")
  then false
  else 
    let csuper = (findClass env cc).super in
    isSubtype env csuper cp

let isInstance env c id =
  let c0 = (findObj env id).myClass in
  isSubtype env c0 c
