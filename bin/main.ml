open Game
open Game__Board

(*To run, run "OCAMLRUNPARAM=b dune exec bin/main.exe" in command line, or make
  play*)

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

let start_game (num_players : int) (terr_list : territory list) =
  ANSITerminal.print_string [ ANSITerminal.green ]
    "Looks like we're ready to get going! \n"
(*TODO: Game Loop*)

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
          [ ANSITerminal.blue; ANSITerminal.Bold ]
          (x ^ ", "
          ^ string_of_int
              (Game__Board.num_troops
                 (Game__Board.get_territory_from_string x terr_list)))
      else ANSITerminal.print_string [ ANSITerminal.white ] x)
    map_list

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
