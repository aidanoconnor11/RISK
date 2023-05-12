open Random
open Board
exception UnknownPlayer

type card = { 
  troop: string;  
  territory: string;
}

let get_troop card = card.troop
let get_card_territory card = card.territory

type player = {
  name : string;
  territories : territory list;
  troops : int;
  deck : card list;
}

let get_name p = p.name
let get_territories p = p.territories
let get_troops p = p.troops
let get_deck p = p.deck

type game_state = {
  players : player list;
  phase : int;
  deck: card list;
  trade_in_ability : bool;
  trade_in_amount : int;
  territories : territory list
}

let get_players g = g.players
let get_phase g = g.phase
let get_game_deck g = g.deck
let get_trade_in_ability g = g.trade_in_ability
let get_trade_in_amount g = g.trade_in_amount
let get_game_territories g = g.territories

type t = game_state

(*For now makes each card Infantry, working on way to fix*)
let init_deck json = 
  let ters = Game__Board.territories_from_file json in
  let ter_names = List.map (fun x -> Game__Board.get_territory_name x) ters in
  let cards = List.map (fun x -> {troop = "Infantry";territory=x}) ter_names in 
  cards


let init_player name t_lst troops d = {
  name = name;
  territories = t_lst;
  troops = troops;
  deck = d
} 

let init_state p d f = {
  players = p;
  phase = 0;
  deck= d;
  trade_in_ability = false;
  trade_in_amount = 0;
  territories = Game__Board.territories_from_file f;
}

let rec get_territory_draft () g  : territory = 
  ANSITerminal.print_string [ ANSITerminal.green ] 
  "Which territory would you like to add troops to?";
  ANSITerminal.print_string [ ANSITerminal.white ] "> ";
  try
    let t1 = read_line () in 
    if (List.exists (fun x -> Game__Board.get_territory_name x = t1) (get_territories(List.hd g.players))) = false then 
    (ANSITerminal.print_string [ ANSITerminal.green ]
    "You do not have possession of this territory. Try again!";
    get_territory_draft () g) else 
    let ter = (Game__Board.get_territory_from_string t1 g.territories) in 
    (ter)
  with exn -> 
    ANSITerminal.print_string [ ANSITerminal.green ]
      "Something went wrong. Try again! \n";
    get_territory_draft () g

let rec get_territory () g  : territory * territory = 
  ANSITerminal.print_string [ ANSITerminal.green ] 
  "From which territory would you like to attack?";
  ANSITerminal.print_string [ ANSITerminal.white ] "> ";
  try
    let t1 = read_line () in 
    if (List.exists (fun x -> Game__Board.get_territory_name x = t1) (get_territories(List.hd g.players))) = false then 
    (ANSITerminal.print_string [ ANSITerminal.green ]
    "You do not have possession of this territory. Try again!";
    get_territory () g) else 
    (ANSITerminal.print_string [ ANSITerminal.green ] 
    "Which territory would you like to attack?";
    ANSITerminal.print_string [ ANSITerminal.white ] "> ";
    let t2 = read_line () in
    let ter1 = (Game__Board.get_territory_from_string t1 g.territories) in
    let neighbors = List.map (fun x -> 
      Game__Board.get_territory_from_string x g.territories) 
      (Game__Board.get_neighbors ter1) in
    if (List.exists (fun x -> 
      Game__Board.get_territory_name x = t2) neighbors)=false then
    (ANSITerminal.print_string [ ANSITerminal.green ]
    "The territory you want to attack is not a neighboring territory
      of the one you're attacking from. Try again!";
    get_territory () g) else
    let ter2 = (Game__Board.get_territory_from_string t2 g.territories) in 
    (ter1,ter2))
  with exn -> 
    ANSITerminal.print_string [ ANSITerminal.green ]
      "Something went wrong. Try again! \n";
    get_territory () g

let rec valid_trade gs =
  let p = List.hd gs.players in
  let card_counts =
    List.fold_left (fun (tc, tt) card ->
        let troop_type = card.troop in
        let territory = card.territory in
        let tc' = (match Hashtbl.find_opt tc troop_type with
            | None -> Hashtbl.add tc troop_type 1; tc
            | Some count -> Hashtbl.replace tc troop_type (count + 1); tc)
        in
        let tt' = (match Hashtbl.find_opt tt territory with
            | None -> Hashtbl.add tt territory 1; tt
            | Some count -> Hashtbl.replace tt territory (count + 1); tt)
        in
        (tc', tt')
      )
      (Hashtbl.create 0, Hashtbl.create 0) p.deck in
  let num_cards = List.length p.deck in
  let valid_combinations = ref [] in
  for i = 0 to (num_cards - 1) do
    for j = (i + 1) to (num_cards - 1) do
      for k = (j + 1) to (num_cards - 1) do
        let card_i = List.nth p.deck i in
        let card_j = List.nth p.deck j in
        let card_k = List.nth p.deck k in
        let combination = [card_i; card_j; card_k] in
        let troop_counts, terr_counts =
          List.fold_left (fun (tc, tt) card ->
              let troop_type = card.troop in
              let territory = card.territory in
              let tc' = Hashtbl.copy tc in
              let tt' = Hashtbl.copy tt in
              let count = Hashtbl.find tc' troop_type in
              Hashtbl.replace tc' troop_type (count - 1);
              let count = Hashtbl.find tt' territory in
              Hashtbl.replace tt' territory (count - 1);
              (tc', tt')
            )
            (card_counts) combination in
        let valid_troop_combination =
          Hashtbl.fold (fun _ count acc -> acc || (count >= 3))
            troop_counts false in
        let valid_territory_combination =
          Hashtbl.fold (fun _ count acc -> acc && (count >= 1))
            terr_counts true in
        if valid_troop_combination && valid_territory_combination then
          valid_combinations := combination :: !valid_combinations
        else ()
      done
    done
  done;
  !valid_combinations

let rec get_cards () g : card * card * card = 
  ANSITerminal.print_string [ ANSITerminal.green ] 
  "Which cards would you like to trade in?";
  ANSITerminal.print_string [ ANSITerminal.white ] "> ";
  try
    let t1 = read_line () in
    let t2 = read_line () in
    let t3 = read_line () in
    let rec card_check lst ter =
      match lst with
      | [] -> {troop = "";territory = ""}
      | h :: t -> if h.territory = ter then h else card_check t ter in
    let c1 = card_check (get_deck (List.hd g.players)) t1 in 
    let c2 = card_check (get_deck (List.hd g.players)) t2 in 
    let c3 = card_check (get_deck (List.hd g.players)) t3 in  
    (c1,c2,c3)
  with exn -> 
    ANSITerminal.print_string [ ANSITerminal.green ]
      "Something went wrong. Try again! \n";
    get_cards () g

(*Capture Helper. Adds [x] troops to territory [ter]*)
let rec update_list lst ter x = 
  match lst with
  | [] -> []
  | h :: t -> if h = ter then 
    (Game__Board.add_armies_to_territory ter (x))::t 
    else h::update_list t ter x 

let turn_change state = 
  let hd_to_tl =
  match state.players with
  | [] -> []
  | hd :: tl -> tl @ [hd] in
  let new_list = hd_to_tl in
  {state with players = new_list}
    
let initial_turn state ter = 
  let new_state = turn_change state in 
  let ter_list = ter::get_territories (List.hd new_state.players) in 
  let player = {(List.hd new_state.players) with territories = ter_list} in 
  {new_state with players = player::(List.tl new_state.players)}

(*Should ensure a,b,c are valid trades*)
let trade state = 
  let rec remove_card lst card =
    match lst with
    | [] -> []
    | h :: t -> if h = card then t else h::(remove_card t card) in
  let cards = get_cards () state in 
  match cards with
  | (a,b,c) -> 
    let new_player_list = 
      remove_card (remove_card (remove_card (get_deck (List.hd state.players)) a) b) c in
    let new_game_list = 
      a::b::c::state.deck in
    let player = {(List.hd state.players) with deck = new_player_list } in
    let g1 = {state with players = player::state.players} in 
  {g1 with deck = new_game_list}  
   
let rec get_trade_choice () =
  ANSITerminal.print_string [ ANSITerminal.green ] 
  "Would you like to trade in your cards for additional troops? Answer 'Yes' or 'No'";
  ANSITerminal.print_string [ ANSITerminal.white ] "> ";
  try
    let x = (read_line ()) in
    if x != "Yes" && x!="No" then (
      ANSITerminal.print_string [ ANSITerminal.green ]
        "Sorry, you must answer 'Yes' or 'No'. Would you like to trade
        in cards for additional troops?\n";
      get_trade_choice ())
    else if x == "Yes" then true 
    else false 
  with exn ->
    ANSITerminal.print_string [ ANSITerminal.green ]
      "hmmm this didn't seem to be a valid input, try again! \n";
    get_trade_choice ()

let rec get_draft_troops () =
  ANSITerminal.print_string [ ANSITerminal.green ] 
  "How many troops would you like to move to this territory?";
  ANSITerminal.print_string [ ANSITerminal.white ] "> ";
  try
    let x = int_of_string (read_line ()) in
    if x < 1 then (
      ANSITerminal.print_string [ ANSITerminal.green ]
        "Sorry, you must move at least one troop the territory. How
        many troops would you like to move?\n";
      get_draft_troops ())
    else x
  with exn ->
    ANSITerminal.print_string [ ANSITerminal.green ]
      "hmmm this didn't seem to be a valid integer- enter the ASCII character \
       of how many troops you're moving, i.e 3 \n";
    get_draft_troops ()

(*Might try to abstract at a later time. Should ensure player has valid trades*)
let rec draft state count num_left = 
  let num_troops = get_draft_troops () in
  let ter = get_territory_draft () state in
  let new_ter_lst = 
    update_list (get_territories (List.hd state.players)) ter num_troops in
  let final_player = 
      {(List.hd state.players) with territories = new_ter_lst} in
  if count = 0 then 
  let updated_state = turn_change state in
  let choice = get_trade_choice () in
  let new_state = 
    match choice with
    | true -> trade updated_state
    | false -> updated_state in
  let ter = get_territory_draft () new_state in
  let new_ter_lst = 
    update_list (get_territories (List.hd new_state.players)) ter num_troops in
  let final_player = 
    {(List.hd new_state.players) with territories = new_ter_lst} in
  let return_state = 
    {state with players = final_player::(List.tl state.players)} in
    draft return_state (count+1) (num_left - num_troops)
  else if num_left > 0 then
    let return_state = 
      {state with players = final_player::(List.tl state.players)} in
    draft return_state (count+1) (num_left - num_troops)
  else 
    {state with players = final_player::(List.tl state.players)}  

let elimination state p = 
  let rec remove = function
  | [] -> raise UnknownPlayer
  | h :: t -> if h = p then t else h::remove t 
in {state with players = (remove state.players)}

let get_player_from_territory g ter =
  let rec check_territories (lst : territory list) =
  match lst with
  | [] -> false
  | h :: t -> if h = ter then true else check_territories t in 
  let rec check_players (lst : player list) =
  match lst with
  | [] -> raise UnknownPlayer
  | h :: t -> if (check_territories h.territories)=true then h
  else check_players t in
  check_players g.players

(*Capture Helper. Removes territory [t2] from territory list [lst]*)
let rec remove lst t2 =
  match lst with
  | [] -> []
  | h :: t -> if h = t2 then t else h::remove t t2

let rec get_num_troops () =
  ANSITerminal.print_string [ ANSITerminal.green ] 
  "How many troops would you like to move to the captured territory?";
  ANSITerminal.print_string [ ANSITerminal.white ] "> ";
  try
    let x = int_of_string (read_line ()) in
    if x < 1 then (
      ANSITerminal.print_string [ ANSITerminal.green ]
        "Sorry, you must move at least one troop to capture a territory. How
        many troops would you like to move?\n";
      get_num_troops ())
    else x
  with exn ->
    ANSITerminal.print_string [ ANSITerminal.green ]
      "hmmm this didn't seem to be a valid integer- enter the ASCII character \
       of how many troops you're moving, i.e 3 \n";
    get_num_troops ()

let capture state t1 t2 = 
  let x = get_num_troops () in
  let p2 = (get_player_from_territory state t2) in
  let p2_update = {p2 with territories = remove p2.territories t2} in
  let rec player_list = function
  | [] -> []
  | h :: t -> if h = p2 then p2_update::t else h::player_list t in
  let g1 = { state with players = player_list state.players} in
  let p1 = (get_player_from_territory state t1) in
  let p1_update = {p1 with territories = 
      t2::(update_list (get_territories (List.hd g1.players)) t1 (-x))} in
  let g2 = {g1 with players = p1_update::(List.tl (get_players g1))} in
  let ter_lst = update_list (get_territories (List.hd g2.players)) t2 x in 
  let p1_final = {p1_update with territories = ter_lst} in
  let final_lst = p1_final::(List.tl (get_players g2)) in
  if List.length p2.territories > 1 then
  { g2 with players = final_lst} else 
  (elimination {g2 with players = final_lst} p2)

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
    one troop from t2. *)
let updated_armies g t1 t2 = 
  if Game__Board.get_territory_numtroops t1 = 0 then (capture g t1 t2) 
    else if Game__Board.get_territory_numtroops t2 = 0 then
    capture g t2 t1 else
    let update_player = {(List.hd g.players) with territories = 
      (update_list (get_territories (List.hd g.players)) t2 (-1))} in
    {g with players = update_player::(List.tl g.players)}
    
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

let rec get_num_dice () =
  ANSITerminal.print_string [ ANSITerminal.green ] 
  "How many dice would you like to roll? If you are the attacking territory,
   you may roll between one and three dice. If you are the defending territory,
   you may roll one or two dice.";
  ANSITerminal.print_string [ ANSITerminal.white ] "> ";
  try
    let x = int_of_string (read_line ()) in
    if x < 1 || x > 3 then (
      ANSITerminal.print_string [ ANSITerminal.green ]
        "Sorry, you must roll at least one dice or at most three dice. How
        many dice would you like to roll? \n";
      get_num_dice ())
    else x
  with exn ->
    ANSITerminal.print_string [ ANSITerminal.green ]
      "hmmm this didn't seem to be a valid integer- enter the ASCII character \
       of how many dice you're rolling, i.e 3 \n";
    get_num_dice ()

let attack state = 
  let d1 = get_num_dice () in 
  let d2 = get_num_dice () in
  let (t1,t2) = get_territory () state in
  (battle_decision state d1 d2 t1 t2)

let rec get_num_troops_fortify () =
  ANSITerminal.print_string [ ANSITerminal.green ] 
  "How many troops would you like to move?";
  ANSITerminal.print_string [ ANSITerminal.white ] "> ";
  try
    let x = int_of_string (read_line ()) in
    if x < 1 then (
      ANSITerminal.print_string [ ANSITerminal.green ]
        "Sorry, you must move at least one troop to fortify. How
        many troops would you like to move?\n";
      get_num_troops_fortify ())
    else x
  with exn ->
    ANSITerminal.print_string [ ANSITerminal.green ]
      "hmmm this didn't seem to be a valid integer- enter the ASCII character \
       of how many troops you're moving, i.e 3 \n";
    get_num_troops_fortify ()
  
let rec get_territory_fortify () g  : territory * territory = 
  ANSITerminal.print_string [ ANSITerminal.green ] 
  "From which territory would you like to fortify?";
  ANSITerminal.print_string [ ANSITerminal.white ] "> ";
  try
    let t1 = read_line () in 
    if (List.exists (fun x -> Game__Board.get_territory_name x = t1) (get_territories(List.hd g.players))) = false then 
    (ANSITerminal.print_string [ ANSITerminal.green ]
    "You do not have possession of this territory. Try again!";
    get_territory_fortify () g) else 
    (ANSITerminal.print_string [ ANSITerminal.green ] 
    "Which territory would you like to move the troops to?";
    ANSITerminal.print_string [ ANSITerminal.white ] "> ";
    let t2 = read_line () in
    if (List.exists (fun x -> Game__Board.get_territory_name x = t2) (get_territories(List.hd g.players))) = false then 
    (ANSITerminal.print_string [ ANSITerminal.green ]
    "You do not have possession of this territory. Try again!";
    get_territory_fortify () g) else
    let ter1 = (Game__Board.get_territory_from_string t1 g.territories) in 
    let ter2 = (Game__Board.get_territory_from_string t2 g.territories) in 
    (ter1,ter2))
  with exn -> 
    ANSITerminal.print_string [ ANSITerminal.green ]
      "Something went wrong. Try again! \n";
    get_territory_fortify () g

(*Implementing without dfs checking neighbors for now*)
let fortify state = 
  let n = get_num_troops_fortify () in
  let (t1,t2) = get_territory_fortify () state in
  let first_ter_lst = update_list 
    (get_territories (List.hd state.players)) t1 (-n) in
  let second_ter_lst = update_list first_ter_lst t2 n in 
  let final_player = 
    {(List.hd state.players) with territories = second_ter_lst} in
  {state with players = final_player::(List.tl state.players)}

let finished_game state =
  let rec check (lst : player list) =
    match lst with 
    | [] -> false
    | h :: t -> if (List.length h.territories) == (List.length state.territories)
      then true else check t 
  in
  check state.players



