open Random

type phase = Setup | Draft | Attack | Fortify
type army = Infantry | Cavalry | Artillery

type territory = {
  name : string;
  num_troops : int;
  neighbors : territory list;
}

type standard_card = {
  troop : army;
  territory : territory;
}

type card = Card of standard_card | Wild

type deck = card list

type dice = One | Two | Three | Four | Five | Six

type player = {
  name : string;
  territories : territory list;
  troops : int;
  deck : deck;
}

type game_state = {
  players : player list;
  current_player : player;
  phase : phase;
  deck: deck;
  trade_in_ability : bool;
  trade_in_amount : int;
}

type t = game_state

(**Teresa TODO*)
let initial_turn state ter = state

(**Teresa TODO*)
let trade state cards = state
  (*let t = match state.current_player.sets with
    |1 -> 4
    |2 -> 6
    |3 -> 8
    |4 -> 10
    |5 -> 12
    |x -> (x - 3) * 5 in

  let deck = match cards with 
  |(x, y, z) ->
    match *)

(**Teresa TODO*)    
let card_exchange state d1 d2 d3 = state    

(**Teresa TODO*)
let rec draft state input choice = state
  (* if choice && state.trade_in_ability then 
    draft (trade state state.current_player.cards) input false else 
  match input with
  | [] -> state
  | (x,y) :: t ->
    let p = Player.add x y in 
    draft {state with current_player = p} t false *)

(**Maps dice to int. Using for dice rolling comparison*)
let d_t_i dice = function
| One -> 1
| Two -> 2
| Three -> 3
| Four -> 4
| Five -> 5
| Six -> 6

(**Rolls to a random dice value*)
let roll : dice = 
  let random_int = Random.int 6 in
  match random_int with
  | 0 -> One
  | 1 -> Two
  | 2 -> Three 
  | 3 -> Four 
  | 4 -> Five 
  | _ -> Six

(**NOT WORKING*)
let max_dice_list (lst : dice list) : int = 
  let rec ints = function
  | [] -> []
  | h :: t -> d_t_i h :: ints t
  in
  List.fold_left max min_int []

(*Aidan TODO Possibly remove d1 and d2 as inputs or switch to ints*)
let rec battle_decision state d1 d2 t1 t2 = 
  let rolls = 
  match (d1,d2) with
  | (3,2) -> ([roll;roll;roll],[roll;roll])
  | (2,2) -> ([roll;roll],[roll;roll])
  | (1,2) -> ([roll],[roll;roll])
  | (3,1) -> ([roll;roll;roll],[roll])
  | _ -> ([],[]) (*For now, eventually will raise exn*)
  in let compare_dice rolls = "" in state (*Will finish but I'm falling asleep*)
  

(*Aidan TODO*)
let capture state t1 t2 armies = state

(*Aidan TODO*)
let attack state t1 t2 = state

(*Teresa TODO*)
let fortify state t1 armies t2 = state

(*Teresa TODO*)
let elimination state p = state
