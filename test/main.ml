open OUnit2
open Game
open Game__Board

(* Helper functions cmp_set_like_lists, pp_string, pp_list taken from A2 *)

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether they are
    equivalent set-like lists. That means checking two things. First, they must
    both be "set-like", meaning that they do not contain any duplicates. Second,
    they must contain the same elements, though not necessarily in the same
    order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  && List.length lst2 = List.length uniq2
  && uniq1 = uniq2

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""
let pp_int i = string_of_int i
let pp_tuple t = "(\"" ^ fst t ^ "\",\"" ^ snd t ^ "\")"


(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

let territory_yojson =
  Yojson.Basic.from_file ("data" ^ Filename.dir_sep ^ "territories_basic.json")

let get_territories_from_continent_test (name : string) (continent : string)
    (f : Yojson.Basic.t) (expected : string list) : test =
  name >:: fun _ ->
  assert (
    cmp_set_like_lists expected
      (List.map get_territory_name
         (get_territories_from_continent (territories_from_file f) continent)))

let get_territory_from_string_test (name : string) (terr_name : string)
    (f : Yojson.Basic.t) (expected : string) : test =
  name >:: fun _ ->
  assert_equal expected
    (get_territory_name
       (get_territory_from_string terr_name (territories_from_file f)))

let board_tests =
  [
    get_territories_from_continent_test "Check territories of North America"
      "North America" territory_yojson
      [
        "Alaska";
        "Northwest Territory";
        "Greenland";
        "Quebec";
        "Eastern US";
        "Western US";
        "Central America";
        "Ontario";
        "Alberta";
      ];
    get_territory_from_string_test "Search for Venezuela" "Venezuela"
      territory_yojson "Venezuela";
  ]

 
let d1 =  (init_deck territory_yojson)
let d1_tuple = List.map (fun x -> (Game.get_troop x, Game.get_card_territory x)) d1 

let deck_test name expected_output =
  name >:: fun _ -> assert_equal ~printer:(pp_list pp_tuple) expected_output d1_tuple

let p1 =
  init_player "Bob"
    (get_territories_from_continent
       (territories_from_file territory_yojson)
       "North America")
    0 d1

let p2 =
  init_player "Dave"
    (get_territories_from_continent
       (territories_from_file territory_yojson)
       "South America")
    0 d1

let europe = (get_territories_from_continent (territories_from_file territory_yojson) "Europe")
let asia = (get_territories_from_continent (territories_from_file territory_yojson) "Asia")
let scandanavia = add_armies_to_territory (get_territory_from_string "Scandanavia" europe) 3
let iceland = add_armies_to_territory (get_territory_from_string "Iceland" europe) 5
let gb = add_armies_to_territory (get_territory_from_string "Great Britain" europe) 7

let china = add_armies_to_territory (get_territory_from_string "China" asia) 2
let india = add_armies_to_territory (get_territory_from_string "India" asia) 4
let t1 = [scandanavia; iceland; gb]
let t2 = [china; india]

let p1 = init_player "Bob" (get_territories_from_continent (territories_from_file territory_yojson) "North America") 0 d1
let p2 = init_player "Dave" (get_territories_from_continent (territories_from_file territory_yojson) "South America") 0 d1
let p3 = init_player "Joe" t2 20 d1 
let p4 = init_player "Matt" t1 12 d1

let player_test name expected_output p =
  name >:: fun _ ->
  assert_equal true (cmp_set_like_lists expected_output (List.map get_territory_name (Game.get_territories p)))

let g1 = init_state [p1;p2] d1 territory_yojson

let g2 = init_state [p4;p3;p1;p2] d1 territory_yojson

let g3 = init_state [p3;p4;p1;p2] d1 territory_yojson

let capture_test 
(name : string)
(state : Game.t)
(t1 : string)
(t2 : string)
(i : int)
(expected_output : string list) : test =
name >:: fun _ ->
  let x = (capture state 
  (get_territory_from_string t1 (territories_from_file  territory_yojson))
  (get_territory_from_string t2 (territories_from_file  territory_yojson))) in
  assert_equal true (cmp_set_like_lists expected_output 
  (List.map get_territory_name 
  (Game.get_territories 
  (List.nth (Game.get_players x) i))))

let capture_territory_troops_test 
(name : string)
(state : Game.t)
(t1 : string)
(t2 : string)
(expected_output : int list) : test =
name >:: fun _ ->
  let new_s = (capture state 
  (get_territory_from_string t1 (territories_from_file  territory_yojson))
  (get_territory_from_string t2 (territories_from_file  territory_yojson))) in
  let player = List.hd (Game.get_players new_s) in
  let list = (List.map Game__Board.get_territory_numtroops (Game.get_territories player)) in 
  assert_equal ~printer:(pp_list pp_int) expected_output list

let battle_decision_test 
(name : string)
(state : Game.t)
(d1 : int)
(d2 : int)
(t1 : Game__Board.territory)
(t2 : Game__Board.territory)
(expected_output : Game.t) : test =
name >:: fun _ ->
  assert_equal expected_output (battle_decision state d1 d2 t1 t2)

let elimination_test (name : string) (state : Game.t) (player : Game.player)
    (expected_output : string list) : test =
  name >:: fun _ ->
  let x = elimination state player in

  let list = (List.map Game.get_name (Game.get_players x)) in
  assert_equal ~printer: (pp_list pp_string) expected_output (list)
 
let update_list_test
(name : string)
(lst : Game__Board.territory list)
(ter : Game__Board.territory)
(x : int) 
(expected_output : int list): test = 
name >:: fun _ ->
  let out = Game.update_list lst ter x in 
  let list = (List.map Game__Board.get_territory_numtroops out) in
  assert_equal ~printer:(pp_list pp_int) expected_output list 

let fortify_test
(name : string)
(state : Game.t)
(expected_output : int list) : test = 
name >:: fun _ ->
  let new_s = fortify state in
  let current_player = List.hd (Game.get_players new_s) in
  let list = List.map Game__Board.get_territory_numtroops (Game.get_territories current_player) in
  assert_equal ~printer:(pp_list pp_int) expected_output list
  

let game_tests =
  [
    deck_test "Initial" [("Cavalry","Alaska"); ("Infantry","Alberta"); ("Artillery","Central America"); ("Cavalry","Eastern US"); ("Infantry","Greenland"); ("Artillery","Northwest Territory"); ("Cavalry","Ontario"); ("Infantry","Quebec"); ("Artillery","Western US"); ("Cavalry","Argentina"); ("Infantry","Brazil"); ("Artillery","Venezuela"); ("Cavalry","Peru"); ("Infantry","Congo"); ("Artillery","Alaska"); ("Cavalry","Alberta"); ("Infantry","Central America"); ("Artillery","Eastern US"); ("Cavalry","Greenland"); ("Infantry","Northwest Territory"); ("Artillery","Ontario"); ("Cavalry","Quebec"); ("Infantry","Western US"); ("Artillery","Argentina"); ("Cavalry","Brazil"); ("Infantry","Venezuela"); ("Artillery","Peru"); ("Cavalry","Congo"); ("Infantry","Alaska"); ("Artillery","Alberta"); ("Cavalry","Central America"); ("Infantry","Eastern US"); ("Artillery","Greenland"); ("Cavalry","Northwest Territory"); ("Infantry","Ontario"); ("Artillery","Quebec"); ("Cavalry","Western US"); ("Infantry","Argentina"); ("Artillery","Brazil"); ("Cavalry","Venezuela"); ("Infantry","Peru"); ("Artillery","Congo")];
    player_test "North America" [
      "Alaska";
      "Northwest Territory";
      "Greenland";
      "Quebec";
      "Eastern US";
      "Western US";
      "Central America";
      "Ontario";
      "Alberta";
    ] p1; 
    player_test "South America" [
      "Argentina";
      "Brazil";
      "Venezuela";
      "Peru"
    ] p2;
    capture_test "Capturing" g1 "Central America" "Venezuela" 0[
      "Alaska";
      "Northwest Territory";
      "Greenland";
      "Quebec";
      "Eastern US";
      "Western US";
      "Central America";
      "Ontario";
      "Alberta";
      "Venezuela";
    ];
    capture_test "Captured" g1 "Central America" "Venezuela" 1[
      "Argentina";
      "Peru";
      "Brazil"
    ]; 

    capture_territory_troops_test "test" g2 "Great Britain" "China" [5; 3; 5; 4];

    elimination_test "Elimination test" g1 p2 ["Bob"];

    elimination_test "Eliminating from a longer list" g2 p1 ["Matt"; "Joe"; "Dave"];

    update_list_test "updating Iceland" t1 iceland 5 [3;10;7];

    update_list_test "updating China" t2 china 12 [14;4];

    fortify_test "Scandanavia to Iceland" g2 [1; 7; 7]; 

    fortify_test "China to India" g3 [1; 5];



]

let suite = "test suite for risk" >::: List.flatten [ board_tests; game_tests ]
let _ = run_test_tt_main suite
