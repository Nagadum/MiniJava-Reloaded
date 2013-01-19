exception ClassAlreadyPresent of string
type tEnv_v
type tEnv_c
type tEnv
type tClasse
val getSuper : tClasse -> Type.t
val makeEnv : tEnv_v -> tEnv_c -> tEnv
val initialEnv : unit -> tEnv
val findVar : tEnv -> string -> Type.t
val isClass : tEnv -> string -> bool
val findClass : tEnv -> string -> tClasse
val addVar : tEnv -> string -> Type.t -> tEnv
val addClass : tEnv -> string -> tEnv
