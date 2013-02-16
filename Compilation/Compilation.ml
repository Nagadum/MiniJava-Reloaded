open TypeEnv
open AST

let rec getArgsName args =
  match args with 
    | [] -> []
    | (aname, _)::others -> aname::(getArgsName others)

let addFun cname fname fbody args env = 
  let fargs = getArgsName args in
  let fid = TypeEnv.getFunID cname fname in
  let f = TypeEnv.makeFun fargs fbody in
  TypeEnv.addFun env fid f

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
      let envWithClasses = TypeEnv.addClass env c.cname newClass in
      let envWithFuns = makeFuns c.cname c.cmethods envWithClasses in
      makeClassEnv others envWithFuns

let compile typesAST =
  makeClassEnv typesAST (TypeEnv.initialEnv())
    
