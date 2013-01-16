type tEnv_v = (string, AST.value) Hashtbl.t

and tEnv = {

    env_t :  ( int * tObject ) list ;
    env_c : ( string * tClasse ) list;
    env_v : tEnv_v ;
    env_f : ( fun_id * tFunction ) list

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

let makeEnv e_t e_c e_v e_f = {
    env_t = e_t;
    env_c = e_c;
    env_v = e_v;
    env_f = e_f;
}

let initialEnv () = 
  makeEnv [] [] (Hashtbl.create 17 : tEnv_v) []

let findVar env = Hashtbl.find (env.env_v)

let defineVar env n t = 
  let new_v = Hashtbl.copy (env.env_v) in
    Hashtbl.add new_v n t; 
    makeEnv env.env_t env.env_c new_v env.env_f
