(* Realise la phase de compilation *)
open TypeEnv
open AST
open Location

(* Definit les attributs des objects d'une classe *)
let rec makeAttrs env attrlist =
  match attrlist with 
    | [] -> []
    | a::others ->
      match a.adefault with
        | Some e -> (a.aname, e)::(makeAttrs env others)
        | None -> match a.atype with
            | "Int" -> 
              let e = { edesc = Val(Int(0)); eloc = Location.none; etype = None} in
              (a.aname,e)::(makeAttrs env others)
            | "String" -> 
              let e = { edesc = Val(String("")); eloc = Location.none; etype = None} in
              (a.aname,e)::(makeAttrs env others)
            | "Boolean" -> 
              let e = { edesc = Val(Boolean(false)); eloc = Location.none; etype = None} in
              (a.aname,e)::(makeAttrs env others)
            | "Null" -> 
              let e = { edesc = Val(Null); eloc = Location.none; etype = None} in
              (a.aname,e)::(makeAttrs env others)
            | _ -> 
              let e = { edesc = Val(Reference(-1)); eloc = Location.none; etype = None} in
              (a.aname,e)::(makeAttrs env others)

let rec getArgsName args =
  match args with 
    | [] -> []
    | (aname, _)::others -> aname::(getArgsName others)

let addFun cname fname fbody args env = 
  let fargs = getArgsName args in
  let fid = TypeEnv.getFunID cname fname in
  let f = TypeEnv.makeFun fargs fbody in
  TypeEnv.addFun env fid f

(* Compile les fonctions d'une classe *)
let rec makeFuns cname funs env =
  match funs with
    | [] -> env
    | f::others ->
      TypeEnv.addFunToClass cname f.mname env;
      let newEnv = addFun cname f.mname f.mbody f.margstype env in
      makeFuns cname others newEnv
        
let rec makeClassEnv typesAST env = 
  match typesAST with
    | [] -> env
    | c::others ->
      let newClass = (TypeEnv.makeClass c.cname c.cparent) in
      (* On ajoute la classes à l'environnement *)
      let envWithClasses = TypeEnv.addClass env c.cname newClass in
      let cattrs = makeAttrs envWithClasses c.cattributes in
      TypeEnv.addAttrsToClass c.cname cattrs envWithClasses ;
      (* On compile les méthodes définies dans la classe *)
      let envWithFuns = makeFuns c.cname c.cmethods envWithClasses in
      (* On effectue le même travail pour les autres classes *)
      makeClassEnv others envWithFuns

let compile typesAST =
  makeClassEnv typesAST (TypeEnv.initialEnv())
    
