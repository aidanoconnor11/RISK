type phase = Draft | Attack | Fortify

(**TODO: Update types as we go*)
type game_state = {
  players : string list;
  current_player : string;
  player_territories : string list;
  phase : phase;
  deck: string list;
  num_troops : int;
  trade_in_ability : bool;
}

type t = game_state

(*let valid_trade cards =
  match cards with
  | (x,y,z) -> 
    match (x.army_type,y.army_type,z.army_type) with
    | (Infantry, Infantry, Infantry) -> 
   
   
if x.army_type = Infantry && y.army_type = Infantry && z.army_type = Infantry then *) 

(**TODO: Implement this*)  
let rec remove_cards lst card =
  match lst with 
  |[] -> []
  |h :: t -> if h = card then t else remove_cards t card


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