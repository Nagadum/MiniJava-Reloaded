type t = (string, Type.t) Hashtbl.t 

let initialEnv () = (Hashtbl.create 17 : t)

let findVar env = Hashtbl.find env

let defineVar env n t = 
  let result = Hashtbl.copy env in
    Hashtbl.add result n t; 
    result
