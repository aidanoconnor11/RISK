type phase = Setup | Draft | Attack | Fortify
type army = Infantry | Cavalry | Artillery
(**TODO: Update types as we go*)
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



(**TODO: Implement this*)  

let trade state cards = 
  let t = match state.current_player.sets with
    |1 -> 4
    |2 -> 6
    |3 -> 8
    |4 -> 10
    |5 -> 12
    |x -> (x - 3) * 5 in

  let deck = match cards with 
  |(x, y, z) ->
    match 

let rec draft state input choice =
  if choice && state.trade_in_ability then 
    draft (trade state state.current_player.cards) input false else 
  match input with
  | [] -> state
  | (x,y) :: t ->
    let p = Player.add x y in 
    draft {state with current_player = p} t false