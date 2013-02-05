exception ClassAlreadyPresent of string
exception MethodAlreadyPresent of string
type tEnv_v
type tEnv_c
type tEnv
type tClasse
type tArg = Type.t * string
type tFun = { fargs : tArg list ; freturn : Type.t }
val getSuper : tClasse -> Type.t
val makeEnv : tEnv_v -> tEnv_c -> tEnv
val initialEnv : unit -> tEnv

val isVar : tEnv -> string -> bool
val findVar : tEnv -> string -> Type.t
val addVar : tEnv -> string -> Type.t -> tEnv

val isClass : tEnv -> string -> bool
val findClass : tEnv -> string -> tClasse
val addClass : tEnv -> string -> tEnv

val isFun : tEnv -> string -> string -> bool
val isFun_rec : tEnv -> string -> string -> bool
val findFun : tEnv -> string -> string -> tFun
val findFun_rec : tEnv -> string -> string -> tFun
val addFun : tEnv -> string -> string -> tFun -> tEnv

val setSuper : tEnv -> string -> string -> tEnv
