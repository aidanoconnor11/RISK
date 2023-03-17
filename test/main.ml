open OUnit2
open Game
open Game__Board

let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  && List.length lst2 = List.length uniq2
  && uniq1 = uniq2

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""


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

let territory_from_continent (name : string) (continent : string)
    (f : Yojson.Basic.t) (expected : string list) : test =
  name >:: fun _ ->
  assert (
    cmp_set_like_lists expected
      (List.map get_territory_name
         (get_territories_from_continent (territories_from_file f) continent)))

let board_tests =
  [
    territory_from_continent "Check territories of North America"
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
  ]

  
let d1 =  (init_deck territory_yojson)
let d1_tuple = List.map (fun x -> (Game.get_troop x, Game.get_territory x)) d1 
let deck_test name expected_output =
  name >:: fun _ ->
  assert_equal expected_output d1_tuple

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

(*Will finish test but am preoccupied*)  
let game_tests = 
  [
    deck_test "Initial" [
      ("Infantry","Alaska"); ("Infantry","Alberta"); ("Infantry","Central America"); ("Infantry","Eastern US"); 
      ("Infantry","Greenland"); ("Infantry","Northwest Territory"); ("Infantry","Alaska"); ("Infantry","Alaska"); 
      ("Infantry","Alaska"); ("Infantry","Alaska"); ("Infantry","Alaska"); ("Infantry","Alaska"); ("Infantry","Alaska"); 
      ("Infantry","Alaska"); ("Infantry","Alaska"); ("Infantry","Alaska"); ("Infantry","Alaska"); 
      ("Infantry","Alaska"); ("Infantry","Alaska"); ("Infantry","Alaska"); ("Infantry","Alaska"); ("Infantry","Alaska"); ("Infantry","Alaska"); 
      ("Infantry","Alaska"); ("Infantry","Alaska"); ("Infantry","Alaska"); ("Infantry","Alaska"); ("Infantry","Alaska"); ("Infantry","Alaska"); 
      ("Infantry","Alaska"); ("Infantry","Alaska"); ("Infantry","Alaska"); ("Infantry","Alaska"); ("Infantry","Alaska"); ("Infantry","Alaska"); 
      ("Infantry","Alaska"); ("Infantry","Alaska"); ("Infantry","Alaska"); ("Infantry","Alaska"); ("Infantry","Alaska"); ("Infantry","Alaska"); ("Infantry","Alaska")
    ] 
]
let suite = "test suite for risk" >::: List.flatten [ board_tests; game_tests ]
let _ = run_test_tt_main suite
