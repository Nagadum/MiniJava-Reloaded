(* Une position dans un fichier *)
type t

(* Une position vide *)
val none : t
(* Initialisation de la position du tampon *)
(* Le second argument est le nom du fichier lu *)
val init : Lexing.lexbuf -> string -> unit

(* La  position courante du tampon *)
val curr : Lexing.lexbuf -> t
(* Incrémentation de la ligne du tampon *)
val incr_line : Lexing.lexbuf -> unit
(* La  position courante du symbole *)
val symbol_loc : unit -> t
(* La  position courante du nieme element de droite *)
val rhs_loc : int -> t

(* Affichage d'une position *)
val print : t -> unit
