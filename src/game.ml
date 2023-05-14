open Random
open Board

exception UnknownPlayer

type card = {
  troop : string;
  territory : string;
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
  mutable phase : int;
  deck : card list;
  trade_in_ability : bool;
  trade_in_amount : int;
  territories : territory list;
}

let get_players g = g.players
let get_phase g = g.phase
let get_game_deck g = g.deck
let get_trade_in_ability g = g.trade_in_ability
let get_trade_in_amount g = g.trade_in_amount
let get_game_territories g = g.territories

type t = game_state

let init_deck (json : Yojson.Basic.t) : card list =
  let ters = Game__Board.territories_from_file json in
  let ter_names = List.map (fun x -> Game__Board.get_territory_name x) ters in
  let num_territories = List.length ter_names in
  let num_cards = num_territories / 3 in
  let cards =
    let rec create_card_list count troop acc =
      if count = 0 then acc
      else
        let new_card = { troop = troop; territory = List.nth ter_names (count-1) } in
        create_card_list (count-1) (match troop with
            | "Infantry" -> "Cavalry"
            | "Cavalry" -> "Artillery"
            | "Artillery" -> "Infantry"
            | _ -> failwith "Invalid troop type"
          ) (new_card::acc)
    in
    create_card_list num_cards "Infantry" []
    @ create_card_list num_cards "Cavalry" []
    @ create_card_list num_cards "Artillery" []
  in
  cards

let init_player name t_lst troops d =
  { name; territories = t_lst; troops; deck = d }

let init_state p d f =
  {
    players = p;
    phase = 0;
    deck = d;
    trade_in_ability = false;
    trade_in_amount = 0;
    territories = Game__Board.territories_from_file f;
  }

let rec get_territory () g : territory * territory =
  ANSITerminal.print_string [ ANSITerminal.green ]
    "From which territory would you like to attack?";
  ANSITerminal.print_string [ ANSITerminal.white ] "> ";
  try
    let t1 = read_line () in
    if
      List.exists
        (fun x -> Game__Board.get_territory_name x = t1)
        (get_territories (List.hd g.players))
      = false
    then (
      ANSITerminal.print_string [ ANSITerminal.green ]
        "You do not have possession of this territory. Try again!";
      get_territory () g)
    else (
      ANSITerminal.print_string [ ANSITerminal.green ]
        "Which territory would you like to attack?";
      ANSITerminal.print_string [ ANSITerminal.white ] "> ";
      let t2 = read_line () in
      let ter1 = Game__Board.get_territory_from_string t1 g.territories in
      let neighbors =
        List.map
          (fun x -> Game__Board.get_territory_from_string x g.territories)
          (Game__Board.get_neighbors ter1)
      in
      if
        List.exists (fun x -> Game__Board.get_territory_name x = t2) neighbors
        = false
      then (
        ANSITerminal.print_string [ ANSITerminal.green ]
          "The territory you want to attack is not a neighboring territory\n\
          \      of the one you're attacking from. Try again!";
        get_territory () g)
      else
        let ter2 = Game__Board.get_territory_from_string t2 g.territories in
        (ter1, ter2))
  with exn ->
    ANSITerminal.print_string [ ANSITerminal.green ]
      "Something went wrong. Try again! \n";
    get_territory () g

let rec valid_trade gs =
  let p = List.hd gs.players in
  let card_counts =
    List.fold_left
      (fun (tc, tt) card ->
        let troop_type = card.troop in
        let territory = card.territory in
        let tc' =
          match Hashtbl.find_opt tc troop_type with
          | None ->
              Hashtbl.add tc troop_type 1;
              tc
          | Some count ->
              Hashtbl.replace tc troop_type (count + 1);
              tc
        in
        let tt' =
          match Hashtbl.find_opt tt territory with
          | None ->
              Hashtbl.add tt territory 1;
              tt
          | Some count ->
              Hashtbl.replace tt territory (count + 1);
              tt
        in
        (tc', tt'))
      (Hashtbl.create 0, Hashtbl.create 0)
      p.deck
  in
  let num_cards = List.length p.deck in
  let valid_combinations = ref [] in
  for i = 0 to num_cards - 1 do
    for j = i + 1 to num_cards - 1 do
      for k = j + 1 to num_cards - 1 do
        let card_i = List.nth p.deck i in
        let card_j = List.nth p.deck j in
        let card_k = List.nth p.deck k in
        let combination = [ card_i; card_j; card_k ] in
        let troop_counts, terr_counts =
          List.fold_left
            (fun (tc, tt) card ->
              let troop_type = card.troop in
              let territory = card.territory in
              let tc' = Hashtbl.copy tc in
              let tt' = Hashtbl.copy tt in
              let count = Hashtbl.find tc' troop_type in
              Hashtbl.replace tc' troop_type (count - 1);
              let count = Hashtbl.find tt' territory in
              Hashtbl.replace tt' territory (count - 1);
              (tc', tt'))
            card_counts combination
        in
        let valid_troop_combination =
          Hashtbl.fold (fun _ count acc -> acc || count >= 3) troop_counts false
        in
        let valid_territory_combination =
          Hashtbl.fold (fun _ count acc -> acc && count >= 1) terr_counts true
        in
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
      | [] -> { troop = ""; territory = "" }
      | h :: t -> if h.territory = ter then h else card_check t ter
    in
    let c1 = card_check (get_deck (List.hd g.players)) t1 in
    let c2 = card_check (get_deck (List.hd g.players)) t2 in
    let c3 = card_check (get_deck (List.hd g.players)) t3 in
    (c1, c2, c3)
  with exn ->
    ANSITerminal.print_string [ ANSITerminal.green ]
      "Something went wrong. Try again! \n";
    get_cards () g

let rec update_list lst ter x =
  match lst with
  | [] -> []
  | h :: t ->
      if Game__Board.get_territory_name h = Game__Board.get_territory_name ter
      then Game__Board.add_armies_to_territory h x :: t
      else h :: update_list t ter x

let turn_change state =
  let hd_to_tl =
    match state.players with
    | [] -> []
    | hd :: tl -> tl @ [ hd ]
  in
  let new_list = hd_to_tl in
  { state with players = new_list }

(*Draft helper that takes the 3 cards being traded in and adds them to the
   game deck*)
let trade state =
  let rec remove_card lst card =
    match lst with
    | [] -> []
    | h :: t -> if h = card then t else h :: remove_card t card
  in
  let cards = get_cards () state in
  match cards with
  | a, b, c ->
      let new_player_list =
        remove_card
          (remove_card (remove_card (get_deck (List.hd state.players)) a) b)
          c
      in
      let new_game_list = a :: b :: c :: state.deck in
      let player = { (List.hd state.players) with deck = new_player_list } in
      let g1 = { state with players = player :: state.players } in
      { g1 with deck = new_game_list }

(*Determines number of troops given for drafting by calculating the number
   of territories and dividing by 3*)
let troops_given state =
  let num_ters = get_territories (List.hd state.players) in
  List.length num_ters / 3

let rec get_trade_choice () =
  ANSITerminal.print_string [ ANSITerminal.green ]
    "Would you like to trade in your cards for additional troops? Answer 'Yes' \
     or 'No'";
  ANSITerminal.print_string [ ANSITerminal.white ] "> ";
  try
    let x = read_line () in
    if x <> "Yes" && x <> "No" then (
      ANSITerminal.print_string [ ANSITerminal.green ]
        "Sorry, you must answer 'Yes' or 'No'. Would you like to trade\n\
        \        in cards for additional troops?\n";
      get_trade_choice ())
    else if x = "Yes" then true
    else false
  with exn ->
    ANSITerminal.print_string [ ANSITerminal.green ]
      "hmmm this didn't seem to be a valid input, try again! \n";
    get_trade_choice ()

let elimination state p =
  let rec remove = function
    | [] -> raise UnknownPlayer
    | h :: t -> if h = p then t else h :: remove t
  in
  { state with players = remove state.players }

let get_player_from_territory g ter =
  let rec check_territories (lst : territory list) =
    match lst with
    | [] -> false
    | h :: t ->
        if Game__Board.get_territory_name h = Game__Board.get_territory_name ter
        then true
        else check_territories t
  in
  let rec check_players (lst : player list) =
    match lst with
    | [] -> raise UnknownPlayer
    | h :: t ->
        if check_territories h.territories = true then h else check_players t
  in
  check_players g.players

(*Capture Helper. Removes territory [t2] from territory list [lst]*)
let rec remove lst t2 =
  match lst with
  | [] -> []
  | h :: t ->
      if get_territory_name h = get_territory_name t2 then t
      else h :: remove t t2

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

let rec list_of_territories (l : territory list) : string =
  match l with
  | [] -> ""
  | h :: t -> Game__Board.get_territory_name h ^ ", " ^ list_of_territories t

let rec owns_continent (local : territory list) (continent : territory list) :
    bool =
  List.for_all (fun x -> List.mem x local) continent

let ignore _ = ()
let possible_move numtroops possible_terrs desired : bool =
  try
    let pairs = String.split_on_char ',' desired in
    let desired_troops =
      List.fold_left
        (fun acc x ->
          acc
          + int_of_string
              (List.hd (List.rev (List.tl (String.split_on_char ' ' x)))))
        0 pairs
    in
    if desired_troops > numtroops then false
    else
      let captured_terr_name_list =
        List.map (fun x -> Game__Board.get_territory_name x) possible_terrs
      in
      let desired_terr_name_list =
        List.map (fun x -> List.hd (String.split_on_char ' ' x)) pairs
      in
      if
        List.exists
          (fun x -> not (List.mem x captured_terr_name_list))
          desired_terr_name_list
      then false
      else true
  with Failure e -> false

let rec get_all_territories (m : player list) : territory list =
  match m with
  | [] -> []
  | h :: t -> h.territories @ get_all_territories t

let rec add_pairs pairs lst =
  match pairs with
  | [] -> ()
  | h :: t ->
      ignore
        (Game__Board.add_armies_to_territory
           (Game__Board.get_territory_from_string (fst h) lst)
           (snd h));
      add_pairs t lst

let string_of_char c = String.make 1 c

(* Converts a string to a list of chars *)
let explode str =
  let rec explode_inner cur_index chars =
    if cur_index < String.length str then
      let new_char = str.[cur_index] in
      explode_inner (cur_index + 1) (chars @ [ new_char ])
    else chars
  in
  explode_inner 0 []

(* Converts a list of chars to a string *)
let rec implode chars =
  match chars with
  | [] -> ""
  | h :: t -> string_of_char h ^ implode t

let rec replace_underscores (s : string) : string =
  match explode s with
  | [] -> ""
  | h :: t ->
      if h = '_' then implode [ ' ' ] ^ replace_underscores (implode t)
      else implode [ h ] ^ replace_underscores (implode t)

let rec draft s =
  let player = List.nth s.players 0 in
  let territory_list_string = list_of_territories player.territories in
  let proper_terr_list_string =
    String.sub territory_list_string 0 (String.length territory_list_string - 2)
  in
  let choice = 
    match (List.length (valid_trade s)) with
    | 0 -> false 
    | _ -> get_trade_choice () in
  let new_s =
    match choice with
      | true -> trade s
      | false -> s
  in
  let num_territories = List.length player.territories in
  ANSITerminal.print_string [ ANSITerminal.white ]
    ("Player "
    ^ string_of_int (int_of_string player.name + 1)
    ^ ": It is your turn! You currently own the following territories: "
    ^ proper_terr_list_string ^ ". That means you get max(3, "
    ^ string_of_int num_territories
    ^ "/3)= "
    ^ string_of_int (max 3 (num_territories / 3))
    ^ " additional troops. Where would you like to place these troops? Note \
       that you are able to place troops at multiple territories, so please \
       enter\n\
       in the following format: 'IBC 3,Level_B 5' if you would like to place 3 \
       on IBC and 5 on Level B, for example.\n\
      \ >");
  let b = read_line () in
  let wanted_places = String.split_on_char ',' b in
  if
    (*TODO: Examine if Possible Move*)
    possible_move (max 3 (num_territories / 3)) player.territories b
  then (
    let pairs =
      List.map
        (fun x ->
          let z = String.split_on_char ' ' x in
          (replace_underscores (List.nth z 0), int_of_string (List.nth z 1)))
        wanted_places
    in
    add_pairs pairs player.territories;
    (new_s, get_all_territories s.players))
  else (
    ANSITerminal.print_string [ ANSITerminal.white ]
      "This doesn't seem to be a valid set of territories! Try again, please\n";
    draft s)

let capture (s : t) (pname : int) (captured : territory) : t =
  let player = List.hd s.players in
  let new_player_val =
    { player with territories = captured :: player.territories }
  in
  let player_captured = Game__Board.get_player_number captured in
  ignore (Game__Board.set_territory_owner captured (int_of_string player.name));
  let player_new =
    List.find (fun x -> int_of_string x.name = player_captured) s.players
  in
  let player_captured_new_list =
    List.filter
      (fun x ->
        String.compare (get_territory_name x) (get_territory_name captured) <> 0)
      player_new.territories
  in
  let player_captured_new =
    { player_new with territories = player_captured_new_list }
  in
  let rec replace_player = function
    | [ a ] -> [ player_captured_new ]
    | h :: t ->
        if h.name = player_captured_new.name then player_captured_new :: t
        else h :: replace_player t
    | _ -> failwith "Invalid"
  in
  let final_list = replace_player s.players in
  { s with players = new_player_val :: List.tl final_list }

(**Checks if either territory has been captured or not and if not it removes one
   troop from t2. *)
let updated_armies g t1 t2 =
  if Game__Board.get_territory_numtroops t1 = 0 then capture g 0 t2
  else if Game__Board.get_territory_numtroops t2 = 0 then capture g 0 t1
  else
    let update_player =
      {
        (List.hd g.players) with
        territories = update_list (get_territories (List.hd g.players)) t2 (-1);
      }
    in
    { g with players = update_player :: List.tl g.players }

let rec attack (s : t) : t * territory list =
  let player = List.nth s.players 0 in
  let territory_list_string = list_of_territories player.territories in
  let proper_terr_list_string =
    String.sub territory_list_string 0 (String.length territory_list_string - 2)
  in
  ANSITerminal.print_string [ ANSITerminal.white ]
    "\n\
     ATTACK!!!!! \n\
    \ Now, you must conquer. This is optional, but there's no way to win \
     without attacking! Would you like to attack? Say 'Yes' if you would like \
     to attack and 'No' if you would just like to move to fortify \n\
     >";
  let b = read_line () in
  if String.compare b "Yes" = 0 then (
    ANSITerminal.print_string [ ANSITerminal.white ]
      ("\n Alright, time to attack. You currently have control of "
     ^ proper_terr_list_string
     ^ ". Now remember, you can only attack nations connected to the one \
        you're attacking from. You also may only attack from a place where you \
        have at least 2 troops. Now, where would you like to attack from? \n\
       \ >");
    try
      let t =
        Game__Board.get_territory_from_string (read_line ()) player.territories
      in
      if Game__Board.get_territory_numtroops t < 2 then (
        ANSITerminal.print_string [ ANSITerminal.white ]
          "This only has one troop!\n";
        attack s)
      else
        let neighbor_territories =
          List.fold_left
            (fun acc x -> x ^ ", " ^ acc)
            ""
            (Game__Board.get_neighbors t)
        in
        let fixed_neighbor_territories =
          String.sub neighbor_territories 0
            (String.length neighbor_territories - 2)
        in
        ANSITerminal.print_string [ ANSITerminal.white ]
          ("Lookin' good, now you can attack any of these: "
         ^ fixed_neighbor_territories ^ " Which would you like to attack? \n>");
        let input = read_line () in
        let neighbor_territory_list =
          List.map
            (fun x ->
              Game__Board.get_territory_from_string x
                (get_all_territories s.players))
            (Game__Board.get_neighbors t)
        in
        let attacked_terr =
          Game__Board.get_territory_from_string input neighbor_territory_list
        in
        ANSITerminal.print_string [ ANSITerminal.white ]
          "\n\
          \ How many troops would you like to attack this territory with? \n\
          \ >";
        let num_troops = read_line () in
        try
          let m = int_of_string num_troops in
          if m >= Game__Board.get_territory_numtroops t then (
            ANSITerminal.print_string [ ANSITerminal.white ]
              "\n That's too many! Try again \n";
            attack s)
          else (
            ignore (Game__Board.add_armies_to_territory t (-m));
            ANSITerminal.print_string [ ANSITerminal.white ]
              "\n\
               Sounds good! Now, the attacking player will decide if they \
               would like to roll one, two, or three die. The defending player \
               will announce if they would like to roll one or two die. Then, \
               we will compare the \n\
              \                die values to see what changes happen to the \
               armies. So, attacking player how many die would you like to \
               roll? \n\
              \ >";
            let attacking_num_die = read_line () in
            let int_attacking_num_die = int_of_string attacking_num_die in
            ANSITerminal.print_string [ ANSITerminal.white ]
              "Now, defending player, how many die would you like to roll?\n>";
            let defending_num_die = read_line () in
            let int_defending_num_die = int_of_string defending_num_die in
            if
              int_attacking_num_die > 3 || int_defending_num_die > 2
              || int_attacking_num_die < 1 || int_attacking_num_die < 1
            then (
              ANSITerminal.print_string [ ANSITerminal.white ]
                "This is an invalid number of die! Try again :( \n";
              attack s)
            else (
              Random.self_init ();
              let attacker_rolls =
                Array.make int_attacking_num_die (Random.int 7)
              in
              let defender_rolls =
                Array.make int_defending_num_die (Random.int 7)
              in
              Array.sort Int.compare attacker_rolls;
              Array.sort Int.compare defender_rolls;
              let attack_num_troops_ref = ref m in
              let looper =
                ref (min int_attacking_num_die int_defending_num_die - 1)
              in
              let roll_result = Array.make (!looper + 1) false in
              while !looper <> 0 do
                if attacker_rolls.(!looper) > defender_rolls.(!looper) then (
                  roll_result.(!looper) <- true;
                  ignore
                    (Game__Board.add_armies_to_territory attacked_terr (-1)))
                else (
                  roll_result.(!looper) <- false;
                  attack_num_troops_ref := !attack_num_troops_ref - 1);
                looper := !looper - 1
              done;
              if
                !attack_num_troops_ref
                > Game__Board.get_territory_numtroops attacked_terr
              then (
                ANSITerminal.print_string [ ANSITerminal.white ]
                  "\n\
                  \ Attack successful! Now, moving on to the next phase, \
                   fortify! \n";
                ignore
                  (Game__Board.add_armies_to_territory attacked_terr
                     (-Game__Board.get_territory_numtroops attacked_terr));
                ignore
                  (Game__Board.add_armies_to_territory attacked_terr
                     !attack_num_troops_ref);
                let new_state =
                  capture s !attack_num_troops_ref attacked_terr
                in
                (new_state, get_all_territories new_state.players))
              else (
                ANSITerminal.print_string [ ANSITerminal.white ]
                  "\n\
                  \ Attack Failed! Sorry, you can't win 'em all. Now on to the \
                   next phase, fortify! \n";
                (s, get_all_territories s.players))))
        with Failure e ->
          ANSITerminal.print_string [ ANSITerminal.white ]
            "That's not an integer! Restart :( \n ";
          attack s
    with UnknownTerritory e ->
      ANSITerminal.print_string [ ANSITerminal.white ]
        "This is not a valid territory- either it doesn't exist or you don't \
         own it! \n";
      attack s)
  else if String.compare b "No" = 0 then (s, get_all_territories s.players)
  else (
    ANSITerminal.print_string [ ANSITerminal.white ]
      "\nIt's a simple yes or no question! Try again.";
    attack s)

let finished_game state =
  let rec check (lst : player list) =
    match lst with
    | [] -> false
    | h :: t ->
        if List.length h.territories == List.length state.territories then true
        else check t
  in
  check state.players

let rec fortify (s : t) =
  if (finished_game s) = false then
  let player = List.nth s.players 0 in
  let territory_list_string = list_of_territories player.territories in
  let proper_terr_list_string =
    String.sub territory_list_string 0 (String.length territory_list_string - 2)
  in
  ANSITerminal.print_string [ ANSITerminal.white ]
    ("Player "
    ^ string_of_int (int_of_string player.name + 1)
    ^ " Now, you can either fortify a location by moving any of your troops \
       from one territory you control to another. Note that you can NOT give \
       up a territory by reducing the amount of troops you have on a location \
       to 0. You can only do this once. Think very carefully, where do you \
       want to move your troops to? Do you want to move them at all? Would you \
       roll the dice, or play the mice. Enter 'No' if you want to opt-out of \
       fortification, and 'Yes' if you would like to opt in\n\
       >");
  let b = read_line () in
  if String.compare b "Yes" = 0 then (
    ANSITerminal.print_string [ ANSITerminal.white ]
      ("A wise decision. Where would you like to move troops from? You \
        currently own the following territories: " ^ proper_terr_list_string
     ^ "\n>");
    let input = read_line () in
    try
      let from =
        Game__Board.get_territory_from_string input player.territories
      in
      ANSITerminal.print_string [ ANSITerminal.white ]
        "Wonderful! Now, where would you like to move troops to?\n >";
      let to_in = read_line () in
      let go_to =
        Game__Board.get_territory_from_string to_in player.territories
      in
      if from = go_to then (
        ANSITerminal.print_string [ ANSITerminal.white ]
          "These are the same! Back to the beginning\n";
        fortify s)
      else (
        ANSITerminal.print_string [ ANSITerminal.white ]
          ("How many troops would you like to move from " ^ input ^ " to "
         ^ to_in ^ "?\n>");
        let num_troops = read_line () in
        try
          let m = int_of_string num_troops in
          if m >= Game__Board.get_territory_numtroops from then (
            ANSITerminal.print_string [ ANSITerminal.white ]
              "Too many troops! \n";
            fortify s)
          else (
            ignore (Game__Board.add_armies_to_territory go_to m);
            ignore (Game__Board.add_armies_to_territory from (-m));
            ( { s with players = List.tl s.players @ [ List.hd s.players ] },
              get_all_territories s.players ))
        with Failure e ->
          ANSITerminal.print_string [ ANSITerminal.white ] "Not a valid int!";
          fortify s)
    with UnknownTerritory e ->
      ANSITerminal.print_string [ ANSITerminal.white ]
        "Something went wrong, back to the beginning :( \n";
      fortify s)
  else if String.compare b "No" = 0 then
    ( { s with players = List.tl s.players @ [ List.hd s.players ] },
      get_all_territories s.players )
  else (
    ANSITerminal.print_string [ ANSITerminal.white ]
      "Please inpute either 'Yes' or 'No'\n";
    fortify s)
  else 
  (s, get_territories (List.hd s.players))

