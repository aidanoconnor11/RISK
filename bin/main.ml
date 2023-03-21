open Game
open Game__Board

(*To run, run "OCAMLRUNPARAM=b dune exec bin/main.exe" in command line, or make
  play*)

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
      Game__Board.territories_from_file
        (Yojson.Basic.from_file ("data" ^ Filename.dir_sep ^ x ^ ".json"))
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
              (Game__Board.num_troops
                 (Game__Board.get_territory_from_string x terr_list)))
      else ANSITerminal.print_string [ ANSITerminal.white ] x)
    map_list;
  ANSITerminal.print_string [ ANSITerminal.white ] "\n"

let rec assign_players (num_players : int)
    (players_num_territories : (int * int ref) list) (num_territories : int)
    (terr_list : territory list) =
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
          terr_list
      else (
        num_territories_roller := !num_territories_roller + 1;
        Game__Board.set_territory_owner h roll
        :: assign_players num_players players_num_territories num_territories t)

let start_game (num_players : int) (terr_list : territory list) =
  ANSITerminal.print_string [ ANSITerminal.green ]
    "Looks like we're ready to get going! \n";
  ANSITerminal.print_string [ ANSITerminal.green ]
    "First, the game will randomly assign you the proper amount of troops for \
     how many players are playing, and put a random amount of them at a random \
     assortment of countries!";
  let new_terr_list =
    assign_players num_players
      [ (0, ref 0); (1, ref 0); (2, ref 0); (3, ref 0); (4, ref 0); (5, ref 0) ]
      (Game__Board.num_territories 0 terr_list)
      terr_list
  in
  print_map "map_attempts.txt" new_terr_list

(* let terr_one = List.hd terr_list in let new_terr_list =
   Game__Board.add_armies_to_territory (Game__Board.set_territory_owner terr_one
   1) 35 :: List.tl terr_list in print_map "map_attempts.txt" new_terr_list *)

let main () =
  (*To print amongus, uncomment this: ANSITerminal.print_string [
    ANSITerminal.blue ] "\n\ \ \ _____________ \n\ \ __| _______ |\n\ \ | | | |
    |\n\ \ | | |_______| |\n\ \ |_| |\n\ \ | ____ |\n\ \ | | | |\n\ \ |___|
    |___|"; *)
  ANSITerminal.print_string [ ANSITerminal.green ]
    "\nWelcome to RISK in OCAML! How many players are playing in your game?\n";
  let num_players = get_num_players () in
  ANSITerminal.print_string [ ANSITerminal.green ]
    "What map would you like to play? Our options are territories_basic \n";
  let board = get_map () in
  print_map "map_attempts.txt" board;
  ANSITerminal.print_string [ ANSITerminal.white ] "\n";
  start_game num_players board

let () = main ()
