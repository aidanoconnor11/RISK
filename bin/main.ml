open Game
open Game__Board

(*To run, run "OCAMLRUNPARAM=b dune exec bin/main.exe" in command line, or make
  play*)
let number_of_players = ref (-1)

let player_color =
  [
    (-1, ANSITerminal.white);
    (0, ANSITerminal.blue);
    (1, ANSITerminal.red);
    (2, ANSITerminal.yellow);
    (3, ANSITerminal.cyan);
    (4, ANSITerminal.green);
    (5, ANSITerminal.magenta);
  ]

let initial_troops =
  [
    (2, Array.make 2 40);
    (3, Array.make 3 35);
    (4, Array.make 4 20);
    (5, Array.make 5 25);
    (6, Array.make 6 20);
  ]

let territories_owned = Array.make 6 []

let rec get_num_players () =
  ANSITerminal.print_string [ ANSITerminal.white ] "> ";
  let x = read_line () in
  try
    if int_of_string x < 2 || int_of_string x > 6 then (
      ANSITerminal.print_string [ ANSITerminal.green ]
        "Sorry, RISK is a 2-6 player game! \n";
      get_num_players ())
    else int_of_string x
  with exn ->
    if String.uppercase_ascii x = "QUIT" then (
      ANSITerminal.print_string [ ANSITerminal.green ] "Goodbye!\n";
      exit 0)
    else
      ANSITerminal.print_string [ ANSITerminal.green ]
        "hmmm this didn't seem to be a valid integer- enter the ASCII \
         character of how many players are playing, i.e 3 \n";
    get_num_players ()

let rec get_map () =
  ANSITerminal.print_string [ ANSITerminal.white ] "> ";
  let x = read_line () in
  try
    let board =
      ( Game__Board.territories_from_file
          (Yojson.Basic.from_file ("data" ^ Filename.dir_sep ^ x ^ ".json")),
        x ^ ".txt" )
    in
    board
  with exn ->
    if String.uppercase_ascii x = "QUIT" then (
      ANSITerminal.print_string [ ANSITerminal.green ] "Goodbye!\n";
      exit 0)
    else
      ANSITerminal.print_string [ ANSITerminal.green ]
        "Hmm this doesn't seem to be a valid map. Try territories_basic \n";
    get_map ()

(*Found on stackoverflow, evaluates to contents of a file as a string*)
let read_whole_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let territories_from_players (players : player list) : territory list =
  List.fold_left (fun acc player -> acc @ (get_territories player)) [] players


let print_map (map_name : string) (terr_list : territory list) : unit =
  let map_string = read_whole_file ("data" ^ Filename.dir_sep ^ map_name) in
  let map_list = String.split_on_char ',' map_string in
  List.iter
    (fun x ->
      if
        List.exists
          (fun z -> String.compare z x = 0)
          (Game__Board.territories_list terr_list)
      then
        ANSITerminal.print_string
          [
            snd
              (List.find
                 (fun z ->
                   fst z
                   = get_player_number
                       (Game__Board.get_territory_from_string x terr_list))
                 player_color);
            ANSITerminal.Bold;
          ]
          (x ^ ", "
          ^ string_of_int
              (Game__Board.get_territory_numtroops
                 (Game__Board.get_territory_from_string x terr_list)))
      else ANSITerminal.print_string [ ANSITerminal.white ] x)
    map_list;
  ANSITerminal.print_string [ ANSITerminal.blue ] "\nPlayer One is Blue";
  ANSITerminal.print_string [ ANSITerminal.red ] ", Player Two is Red";
  if !number_of_players > 2 then
    ANSITerminal.print_string [ ANSITerminal.yellow ] ", Player Three is Yellow";
  if !number_of_players > 3 then
    ANSITerminal.print_string [ ANSITerminal.cyan ] ", Player Four is Cyan";
  if !number_of_players > 4 then
    ANSITerminal.print_string [ ANSITerminal.green ] ", Player Five is Green";
  if !number_of_players > 5 then
    ANSITerminal.print_string [ ANSITerminal.magenta ] ", Player Six is Magenta";
  ANSITerminal.print_string [ ANSITerminal.white ] "\n"

let rec assign_players (num_players : int)
    (players_num_territories : (int * int ref) list) (num_territories : int)
    (initial_troops : int array) (terr_list : territory list) =
  Random.self_init ();
  match terr_list with
  | [] -> []
  | h :: t ->
      let roll = Random.int num_players in
      let num_territories_roller =
        snd (List.find (fun z -> fst z = roll) players_num_territories)
      in
      if
        num_territories mod num_players = 0
        && !num_territories_roller >= num_territories / num_players
        || !num_territories_roller >= (num_territories / num_players) + 1
      then
        assign_players num_players players_num_territories num_territories
          initial_troops terr_list
      else (
        num_territories_roller := !num_territories_roller + 1;
        initial_troops.(roll) <- initial_troops.(roll) - 1;
        ignore (Game__Board.add_armies_to_territory h 1);
        ignore (Game__Board.set_territory_owner h roll);
        territories_owned.(roll) <- h :: territories_owned.(roll);
        h
        :: assign_players num_players players_num_territories num_territories
             initial_troops t)

let rec put_troops_here color (t : territory) (num_players : int)
    (player_num : int) (input : string) : unit =
  try
    let want_troops_int = int_of_string input in
    if
      want_troops_int < 0
      || want_troops_int
         > (snd (List.find (fun x -> fst x = num_players) initial_troops)).(player_num)
    then (
      ANSITerminal.print_string [ color ]
        "This is not a valid amount of troops, either it is negative or you do \
         not have enough troops. Please try again \n\
        \ > ";
      put_troops_here color t num_players player_num (read_line ()))
    else (
      (snd (List.find (fun x -> fst x = num_players) initial_troops)).(player_num) <-
        (snd (List.find (fun x -> fst x = num_players) initial_troops)).(player_num)
        - want_troops_int;
      ignore (Game__Board.add_armies_to_territory t want_troops_int))
  with exn ->
    if String.compare (String.uppercase_ascii input) "QUIT" = 0 then (
      ANSITerminal.print_string [ color ] "Goodbye!\n";
      exit 0)
    else
      ANSITerminal.print_string [ color ]
        "This doesn't seem to be a valid integer. Please input the ASCII \
         representation of an integer \n\
        \ > ";
    put_troops_here color t num_players player_num (read_line ())

(* let players_assign_troops (num_players : int) (terr_list : territory list) :
   territory list = let next_list : territory list ref = ref [] in let looper =
   ref num_players in let first_player = ref (Random.int num_players) in while
   !looper > 0 do let lst = territories_owned.(!first_player) in let color = snd
   (List.find (fun z -> fst z = !first_player) player_color) in
   ANSITerminal.print_string [ color ] ("\nPlayer " ^ string_of_int
   (!first_player + 1) ^ ": "); List.iter (fun x -> ANSITerminal.print_string [
   snd (List.find (fun z -> fst z = !first_player) player_color) ] ("You have
   control of " ^ (Game__Board.get_territory_name x ^ ", and have ") ^
   string_of_int (snd (List.find (fun x -> fst x = num_players)
   initial_troops)).(!first_player) ^ " troops left. How many would you like to
   put here? \n > "); let input = read_line () in next_list := put_troops_here
   color x num_players !first_player input :: !next_list) lst; first_player :=
   (!first_player + 1) mod num_players; looper := !looper - 1 done;
   !next_list *)
let rec mutable_player_assign_troops (num_players : int)
    (terr_list : territory list) map_name : territory list =
  Random.self_init ();
  let looper = ref num_players in
  let first_player = ref (Random.int num_players) in
  while !looper <> 0 do
    let terr_owned = territories_owned.(!first_player) in
    let color = snd (List.find (fun z -> fst z = !first_player) player_color) in
    ANSITerminal.print_string [ color ]
      ("\nPlayer " ^ string_of_int (!first_player + 1) ^ ": ");
    List.iter
      (fun x ->
        ANSITerminal.print_string [ color ]
          ("You have control of "
          ^ Game__Board.get_territory_name x
          ^ " and have "
          ^ string_of_int
              (snd (List.find (fun x -> fst x = num_players) initial_troops)).(!first_player)
          ^ " troops left. How many would you like to put here? \n > ");
        let input = read_line () in
        put_troops_here color x num_players !first_player input;
        ANSITerminal.erase Screen;
        print_map map_name terr_list)
      terr_owned;
    first_player := (!first_player + 1) mod num_players;
    looper := !looper - 1
  done;
  terr_list

let ignore _ = ()

let start_game (num_players : int) (terr_list : territory list)
    (map_name : string) =
  ANSITerminal.print_string [ ANSITerminal.green ]
    "Looks like we're ready to get going! \n";
  ANSITerminal.print_string [ ANSITerminal.green ]
    "First, the game will randomly assign you the proper amount of troops for \
     how many players are playing and give you control of a proportionate \
     amount of countries. Then, it will be up to you to place your remaining \
     troops on the territories you control, based on locations and strategy. \
     The game will roll to decide who goes first!";
  ANSITerminal.print_string [ ANSITerminal.green ]
    "\nHit Enter when you understand and are ready to begin!\n";
  ignore (read_line ());
  let new_terr_list =
    assign_players num_players
      [ (0, ref 0); (1, ref 0); (2, ref 0); (3, ref 0); (4, ref 0); (5, ref 0) ]
      (Game__Board.num_territories 0 terr_list)
      (snd (List.find (fun x -> fst x = num_players) initial_troops))
      terr_list
  in
  print_map map_name new_terr_list;
  (*TODO: Improve this: *)
  let init_board =
    mutable_player_assign_troops num_players new_terr_list map_name
  in
  print_map map_name init_board;
  init_board

(* let terr_one = List.hd terr_list in let new_terr_list =
   Game__Board.add_armies_to_territory (Game__Board.set_territory_owner terr_one
   1) 35 :: List.tl terr_list in print_map "map_attempts.txt" new_terr_list *)

let players_from_territories (ters : territory list) : player list = 
  let players = ref [] in
  players := [
    Game.init_player (string_of_int 0) [] 0 [];
    Game.init_player (string_of_int 1) [] 0 [];
    Game.init_player (string_of_int 2) [] 0 [];
    Game.init_player (string_of_int 3) [] 0 [];
    Game.init_player (string_of_int 4) [] 0 [];
    Game.init_player (string_of_int 5) [] 0 [];
  ];
  List.iter (fun t ->
    let player_index = (get_player_number t) in
    let player = List.nth !players player_index in
    let updated_territories = t :: get_territories player in
    let updated_player = Game.init_player 
      (get_name player) 
      updated_territories
      (get_troops player) 
      (get_deck player)
    in
    List.iteri (fun i p ->
      if i = player_index then players := updated_player :: !players else ()
    ) !players
  ) ters;
  let filtered = List.filter (fun x -> (List.length (get_territories x))> 0) !players in
  filtered

let rec play game board = 
  match Game.finished_game game with 
  | true ->
    print_endline ("Congratulations! You have conquered the world!");
    ()
  | false ->
    print_map (snd board) (territories_from_players (get_players game));
    match Game.get_phase game with
    | 0 -> play (Game.draft game 0 0) board
    | 1 -> play (Game.attack game) board
    | 2 -> play (Game.fortify game) board 
    | _ -> play game  board
    
let main () =
  (*To print amongus, uncomment this: ANSITerminal.print_string [
    ANSITerminal.blue ] "\n\ \ \ _____________ \n\ \ __| _______ |\n\ \ | | | |
    |\n\ \ | | |_______| |\n\ \ |_| |\n\ \ | ____ |\n\ \ | | | |\n\ \ |___|
    |___|"; *)
  ANSITerminal.print_string [ ANSITerminal.green ]
    "\n\
     Welcome to RISK in OCAML! Enter quit at any point to quit the game. \n\
     How many players are playing in your game?\n";
  let num_players = get_num_players () in
  number_of_players := num_players;
  ANSITerminal.print_string [ ANSITerminal.green ]
    "What map would you like to play? Our options are territories_basic and \
     cornell_map \n";
  let board = get_map () in
  ANSITerminal.print_string [ ANSITerminal.white ] "\n";
  let players = players_from_territories(start_game num_players (fst board) (snd board)) in
  play (Game.init_state players [] 
  (Yojson.Basic.from_file ("data" ^ Filename.dir_sep ^ "territories_basic.json"))) board
let () = main ()
