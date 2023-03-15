(** The abstract type of values representing game state*)
type t


type card
type deck
type territory
type player

(**TODO: Documentation*)
val init_player : string -> territory list -> int -> deck -> player

(** [init_state p d] is the initial game state after a player list [p] and a
    deck [d] are passed in from main*)
val init_state : player list -> deck -> t

(** [initial_turn g t] is the resulting game state from [g] after a player
    puts a troop in a territory [t] at the start of the game when troops
    are being placed TERESA*)
val initial_turn : t -> territory -> t

(** [trade g c] is the resulting game state from [g] after the player 
    makes a trade with 3 cards [c] that are predetermined as a valid
    trade TERESA*)
val trade : t -> card * card * card -> t 

(** [card_exchange g d1 d2 d3] is the resulting game state from [g] after
    an exhange of cards [d3] happens between one deck [d1] and another 
    deck [d2] TERESA*)
val card_exchange : t -> deck -> deck -> deck -> t

(** [draft g t b] is the resulting game state from [g] after the player 
    drafts a certain number of troops to territories [t] and trades in 
    cards given the choice to [b] TERESA*)
val draft : t -> (territory * int) list -> bool -> t

(** [attack g t1 t2] is the resulting game state from [g] after the player
    attacks a territory [t2] from a connecting territory [t1] AIDAN TODO*)
val attack : t -> territory -> territory -> t

(** [capture g t1 t2 a] is the resulting game state from [g] after the player 
    captures a territory [t2] with a certain number [a] troops from an 
    attacking territory [t1] AIDAN TODO*)
val capture : t -> territory -> territory -> int -> t

(** [battle_decision g d1 d2 t1 t2] is the resulting game state from [g]
    after the player attacks a defending territory [t2] with # of dice [d2] 
    from a territory [t1] with # of dice [d1] AIDAN TODO*)
val battle_decision : t -> int -> int -> territory -> territory -> t

(** [fortify g t1 a t2] is the resulting game state from [g] after the 
    player fortifies territory [t2] with a certain number of troops [a]
    from territory [t1] TERESA*)
val fortify : t -> territory -> int -> territory -> t

(** [elimination g p] is the resulting game state from [g] after the current
    player eliminates a player [p] TERESA*)
val elimination : t -> player -> t