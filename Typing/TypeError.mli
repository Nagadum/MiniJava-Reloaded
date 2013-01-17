type t

val report_error : t -> unit
exception Error of t * Location.t

val not_subtype : string -> string -> Location.t -> 'a
val not_castable : string -> string -> Location.t -> 'a
val type_clash : string -> Location.t -> 'a
val attribute_clash : string -> Location.t -> 'a
val unknown_type : string -> Location.t -> 'a
val unknown_meth : string -> string -> Location.t -> 'a
val inheritance_cycle : string -> string -> Location.t -> 'a
val too_much_args : Location.t -> 'a
val not_enough_args : Location.t -> 'a
val incorrect_type : string -> string -> Location.t -> 'a
