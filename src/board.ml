module type Country = sig
  type country
end

module CountryImpl : Country = struct
  type continent =
    | North_America of string
    | South_America of string
    | Europe of string
    | Africa of string
    | Australia of string
    | Asia of string

  let valid_territories =
    [
      [
        North_America "Alaska";
        North_America "Alberta";
        North_America "Central America";
        North_America "Eastern US";
        North_America "Greenland";
        North_America "Northwest Territory";
        North_America "Ontario";
        North_America "Quebec";
        North_America "Western US";
      ];
      [
        South_America "Argentina";
        South_America "Brazil";
        South_America "Venezuela";
        South_America "Peru";
      ];
      [
        Europe "Great Britain";
        Europe "Iceland";
        Europe "Northern Europe";
        Europe "Scandinavia";
        Europe "Southern Europe";
        Europe "Ukraine";
        Europe "Western Europe";
      ];
      [
        Asia "Afghanistan";
        Asia "China";
        Asia "India";
        Asia "Irkutsk";
        Asia "Japan";
        Asia "Kamchatka";
        Asia "Middle East";
        Asia "Mongolia";
        Asia "Siam";
        Asia "Siberia";
        Asia "Ural";
        Asia "Yakutsk";
      ];
      [
        Africa "Congo";
        Africa "East Africa";
        Africa "Egypt";
        Africa "Madagascar";
        Africa "North Africa";
        Africa "South Africa";
      ];
      [
        Australia "Eastern Australia";
        Australia "New Guinea";
        Australia "Indonesia";
        Australia "Western Australia";
      ];
    ]

  type country = {
    player_name_territory_numTroops : (string * continent) * int;
  }
end

module type Move = sig
  type move

  val player_moving : move -> string
  val army_type : move -> string (*Placeholder*)
  val country_attacking : move -> CountryImpl.country
  val valid_move : move -> bool
end

module MoveImpl : Move = struct
  type move = {
    attacking_player : string;
    army_type : string;
    country_attacking : CountryImpl.country;
  }

  let player_moving mv = mv.attacking_player
  let army_type mv = mv.army_type
  let country_attacking mv = mv.country_attacking
  let valid_move mv = false (* Must check that mv is valid*)
end

module type Board = sig
  type board

  val init_board : int -> board
  val advance_board : MoveImpl.move -> board -> board
end

module BoardImpl : Board = struct
  type board = {
    num_Players : int;
    countries : CountryImpl.country list;
  }

  let init_board num = { num_Players = num; (*Placeholder*)
                                            countries = [] }

  let advance_board mv current_board = current_board
end
