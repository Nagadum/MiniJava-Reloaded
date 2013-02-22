exception TypeEnvError of string;;

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

    myClass : string ;
    attrs : (string, AST.value) Hashtbl.t

}

and tConst = tEnv -> int -> (AST.expression -> AST.value) -> tObject

and tClasse = {

    super : string ;
    const : tConst ;
    functions : (string, fun_id) Hashtbl.t ;
    cattrs : (string, AST.expression) Hashtbl.t ;

}

and fun_id = string

and tFunction = {

  fargs : string list ;
  fbody : AST.expression

}


(*Recherches dans l'environnement*)

let findObj env = Hashtbl.find (env.env_t)

let findClass env = Hashtbl.find (env.env_c)

let findVar env = Hashtbl.find (env.env_v)

let findFun env = Hashtbl.find (env.env_f)

(* Instanciations des types *)

let initAttr o f aname aexpr =
  Hashtbl.add o.attrs aname (f aexpr)

(* Ajoute les attributs à l'objet *)
let rec addAttrs o env f c =
  Hashtbl.iter (initAttr o f) c.cattrs;
  try
    let cparent = (findClass env c.super) in
    addAttrs o env f cparent
  with Not_found -> ()

let makeObject cname env p f =
  let c = findClass env cname in
  let o = 
    {
      myClass = cname ;
      attrs = (Hashtbl.create 17 : (string, AST.value) Hashtbl.t)
    } in
  Hashtbl.add o.attrs "this" (AST.Reference(p)) ;
  addAttrs o env f c ;
  o

(* Definit le constructeur d'une classe donnée *)
let makeConst cname =
  makeObject cname

let makeClass cname supername = {
  super = supername;
  const = makeConst cname;
  functions = (Hashtbl.create 17 : (string, fun_id) Hashtbl.t);
  cattrs = (Hashtbl.create 17 : (string, AST.expression) Hashtbl.t)
}

let makeFun args body = {
  fargs = args;
  fbody = body
}

let makeEnv e_t e_c e_v e_f i = {
    env_t = e_t;
    env_c = e_c;
    env_v = e_v;
    env_f = e_f;
    next = i
}

(*Modifications de l'environnement*)

let initialEnv () = 
  makeEnv (Hashtbl.create 17 : tEnv_t) (Hashtbl.create 17 : tEnv_c) (Hashtbl.create 17 : tEnv_v) (Hashtbl.create 17 : tEnv_f) 0

let makeCallEnv oldEnv =
  makeEnv oldEnv.env_t oldEnv.env_c (Hashtbl.create 17 : tEnv_v) oldEnv.env_f oldEnv.next

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

let printFunID fname fid =
  print_string (fname ^ "(" ^ fid ^ "), ")

let printAttr name value =
  print_string (name ^ ", ")
    
let printClass cname c =
  print_endline ("---\nClass : " ^ cname);
  print_endline ("extends " ^ c.super);
  print_string "Methods : " ;
  Hashtbl.iter printFunID c.functions;
  print_string "\nAttributs : ";
  Hashtbl.iter printAttr c.cattrs;
  print_endline ""

let printFunction fid f =
  print_endline("---\nfunction : " ^ fid);
  print_endline "Args: ";
  List.iter print_endline f.fargs

let printObj p o =
  print_string("---\nObject : ");
  print_int p ;
  print_endline("\nClass : " ^ o.myClass)

let printEnv env =
  print_endline "----Classes----";
  Hashtbl.iter printClass env.env_c;
  print_endline "----Functions----";
  Hashtbl.iter printFunction env.env_f

(* Instantiations *)

let newObject env f cname =
  let id = env.next in
  env.next <- (id + 1);
  let c = findClass env cname in
  let obj = c.const env id f in
  Hashtbl.add env.env_t id obj;
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

let getType env id =
   (findObj env id).myClass

let isInstance env c id =
  let c0  = getType env id in
  isSubtype env c0 c

let rec getFun env cname fname =
  let c = findClass env cname in
  try
    let fid = Hashtbl.find (c.functions) fname in
    findFun env fid
  with Not_found ->
    getFun env c.super fname

(* Gestion des methodes *)

let getFunID cname fname =
  (cname ^ "@" ^ fname)

let addFunToClass cname fname env =
  let c = findClass env cname in
  Hashtbl.add c.functions fname (getFunID cname fname)

(* Gestion des attributs *)

let rec addAttrsToClass cname attrs env =
  match attrs with 
    | [] -> ()
    | (aname, avalue)::others ->
        let c = findClass env cname in
        Hashtbl.add c.cattrs aname avalue;
        addAttrsToClass cname others env

let addAttrsToEnv env p =
  let o = findObj env p in 
  let new_v = Hashtbl.copy (env.env_v) in
  Hashtbl.iter (Hashtbl.add new_v) o.attrs;
  makeEnv env.env_t env.env_c new_v env.env_f env.next

let setAttr env name value =
  match (findVar env "this") with
    | AST.Reference p -> 
      let o = findObj env p in
      Hashtbl.replace o.attrs name value ;
      Hashtbl.replace env.env_v name value
    | _ -> raise (TypeEnvError("'this' is not a reference"))
