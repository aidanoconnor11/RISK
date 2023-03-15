open Game
open Game__Board

(*To run, run "OCAMLRUNPARAM=b dune exec bin/main.exe" in command line *)

let rec get_num_players () =
  ANSITerminal.print_string [ ANSITerminal.white ] "> ";
  try
    let x = int_of_string (read_line ()) in
    if x < 2 then (
      ANSITerminal.print_string [ ANSITerminal.green ]
        "Sorry, RISK is a multiplayer game! Please enter a number greater than \
         1 \n";
      get_num_players ())
    else x
  with exn ->
    ANSITerminal.print_string [ ANSITerminal.green ]
      "hmmm this didn't seem to be a valid integer- enter the ASCII character \
       of how many players are playing, i.e 3 \n";
    get_num_players ()

let rec get_map () =
  ANSITerminal.print_string [ ANSITerminal.white ] "> ";
  try
    let x = read_line () in
    let board =
      Game__Board.territories_from_file
        (Yojson.Basic.from_file ("data" ^ Filename.dir_sep ^ x ^ ".json"))
    in
    board
  with exn ->
    ANSITerminal.print_string [ ANSITerminal.green ]
      "Hmm this doesn't seem to be a valid map. Try territories_basic \n";
    get_map ()

let start_game (num_players : int) (terr_list : territory list) =
  ANSITerminal.print_string [ ANSITerminal.green ]
    "Looks like we're ready to get going! \n"
(*TODO: Game Loop*)

let main () =
  ANSITerminal.print_string [ ANSITerminal.green ]
    "\nWelcome to RISK in OCAML! How many players are playing in your game?\n";
  let num_players = get_num_players () in
  ANSITerminal.print_string [ ANSITerminal.green ]
    "What map would you like to play? Our options are territories_basic \n";
  let board = get_map () in
  start_game num_players board

let () = main ()
