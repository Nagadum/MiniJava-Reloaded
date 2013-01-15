type t
val initialEnv : unit -> t
val findVar : t -> string -> Type.t
val defineVar : t -> string -> Type.t -> t

