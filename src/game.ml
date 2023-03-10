open Random
open Board

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

let init_state p d= {
  players = p;
  current_player = List.hd p;
  phase = Setup;
  deck= d;
  trade_in_ability = false;
  trade_in_amount = 0;
}
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

(*Aidan TODO*)
let attack state t1 t2 = state

(*Aidan TODO*)
let capture state t1 t2 armies = state

(**Rolls to a random int value between 1 and 6 inclusive*)
let roll : int = 
  let random_int = Random.int 6 in
  match random_int with
  | 0 -> 1
  | 1 -> 2
  | 2 -> 3 
  | 3 -> 4 
  | 4 -> 5 
  | _ -> 6

(*Sorts given list to be in descending order*)  
let sorted_dice_list (lst : int list) : int list = 
  List.rev (List.sort compare lst)

(**Checks if either territory has been captured or not and if not it removes 
    one troop from t2. NOTE: Capture takes in a number of armies as input but
    we don't know that number in battle_decision. Need to get it as user input.
    Also requires a Board function that can remove troops from a territory*)
let updated_armies g t1 t2 = 
    if t1.num_troops = 0 then (capture g t1 t2 5 (*5 is placeholder*)) else if t2.num_troops = 0 then
    capture g t2 t1 5 else g

(*Aidan TODO Possibly remove d1 and d2 as inputs or switch to ints*)
let battle_decision state d1 d2 t1 t2 = 
  let rolls = 
  match (d1,d2) with
  | (3,2) -> ([roll;roll;roll],[roll;roll])
  | (2,2) -> ([roll;roll],[roll;roll])
  | (1,2) -> ([roll],[roll;roll])
  | (3,1) -> ([roll;roll;roll],[roll])
  | _ -> ([],[]) (*For now, eventually will raise exn*)
  in let rec compare_dice g rolls = 
    let first = sorted_dice_list (fst rolls) in
    let second = sorted_dice_list (snd rolls) in
    if first = [] || second = [] then g else
    if List.hd first > List.hd second then 
    compare_dice (updated_armies g t1 t2) (List.tl first,List.tl second) else
    compare_dice (updated_armies g t2 t1) (List.tl first,List.tl second) 
  in compare_dice state rolls

(*Teresa TODO*)
let fortify state t1 armies t2 = state

(*Teresa TODO*)
let elimination state p = state
