open TypeEnv
open AST

let rec makeClassEnv typesAST env = 
  match typesAST with
    | [] -> env
    | c::others ->
      let newClass = (TypeEnv.makeClass c.cname c.cparent) in
      let newEnv = TypeEnv.addClass env c.cname newClass in
      makeClassEnv others newEnv

let compile typesAST =
  makeClassEnv typesAST (TypeEnv.initialEnv())
