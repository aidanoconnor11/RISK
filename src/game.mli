(** The abstract type of values representing game state*)
type t

(**TODO: Documentation*)
val trade : t -> card * card * card -> t 

(** [draft g t b] is the resulting game state from [g] after the player 
    drafts a certain number of troops to territories [t] and trades in 
    cards given the choice to [b] *)
val draft : t -> (territory * int) list -> bool -> t

(**TODO: Documentation*)
val attack : t -> territory -> territory -> t

(**TODO: Documentation*)
val fortify : t -> territory -> int -> territory -> t