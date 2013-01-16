exception ClassAlreadyPresent of string
type tEnv_v
type tEnv_c
type tEnv
val makeEnv : tEnv_v -> tEnv_c -> tEnv
val initialEnv : unit -> tEnv
val findVar : tEnv -> string -> Type.t
val findClass : tEnv -> string -> bool
val addVar : tEnv -> string -> Type.t -> tEnv
val addClass : tEnv -> string -> tEnv
