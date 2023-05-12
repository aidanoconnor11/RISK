open Board
(** The abstract type of values representing game state*)
type t


type card

type player

val get_troop : card -> string
val get_card_territory : card -> string

val get_name : player -> string
val get_territories : player -> territory list
val get_troops : player -> int
val get_deck : player -> card list

val get_players : t -> player list
val get_phase : t -> int
val get_game_deck : t -> card list
val get_trade_in_ability : t -> bool
val get_trade_in_amount : t -> int
val get_game_territories : t -> territory list

val init_deck : Yojson.Basic.t -> card list
val init_player : string -> Game__Board.territory list -> int -> card list -> player

(** [init_state p d] is the initial game state after a player list [p] and a
    deck [d] are passed in from main*)
val init_state : player list -> card list -> Yojson.Basic.t -> t

(** [valid_trade g] returns a list of card lists that are valid trades to make
    with a given game state [g]*)
val valid_trade : t -> card list list

(** [initial_turn g t] is the resulting game state from [g] after a player
    puts a troop in a territory [t] at the start of the game when troops
    are being placed TERESA*)
val initial_turn : t -> Game__Board.territory -> t

(** [trade g c] is the resulting game state from [g] after the player 
    makes a trade with 3 cards [c] that are predetermined as a valid
    trade*)
val trade : t -> t 

(** [draft g b] is the resulting game state from [g] after the player 
    drafts a certain number of troops to territories [t] and trades in 
    cards given the choice to *)
val draft : t -> int -> int -> t

(** [elimination g p] is the resulting game state from [g] after the current
    player eliminates a player [p] *)
val elimination : t -> player -> t

val update_list : Game__Board.territory list -> Game__Board.territory -> int -> Game__Board.territory list

(** [capture g t1 t2 a] is the resulting game state from [g] after the player 
    captures a territory [t2] with a certain number [a] troops from an 
    attacking territory [t1] *)
val capture : t -> Game__Board.territory -> Game__Board.territory -> t

(** [battle_decision g d1 d2 t1 t2] is the resulting game state from [g]
    after the player attacks a defending territory [t2] with # of dice [d2] 
    from a territory [t1] with # of dice [d1] *)
val battle_decision : t -> int -> int -> Game__Board.territory -> Game__Board.territory -> t

(** [attack g] is the resulting game state from [g] after the player
    attacks a territory from a connecting territory*)
val attack : t -> t

(** [fortify g t1 a t2] is the resulting game state from [g] after the 
    player fortifies one territory with a certain number of troops
    from another territory *)
val fortify : t -> t

(** [finished_game g] checks if anyone has won the game in its current state 
    [g] and returns a bool to indicate whether or not it has*)
val finished_game : t -> bool
