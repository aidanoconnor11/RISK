exception UnknownTerritory of string
exception NotNeighbors of string

type territory = {
  name : string;
  continent : string;
  num_troops : int;
  neighbors : string list;
  player : int;
}

let rec get_list (json : Yojson.Basic.t list) : territory list =
  match json with
  | [] -> []
  | h :: t ->
      let n =
        Yojson.Basic.to_string
          (List.assoc "name" (Yojson.Basic.Util.to_assoc h))
      in
      let c =
        Yojson.Basic.to_string
          (List.assoc "continent" (Yojson.Basic.Util.to_assoc h))
      in
      {
        name = String.sub n 1 (String.length n - 2);
        num_troops = 0;
        continent = String.sub c 1 (String.length c - 2);
        neighbors =
          List.map Yojson.Basic.Util.to_string
            (Yojson.Basic.Util.to_list
               (List.assoc "neighbors" (Yojson.Basic.Util.to_assoc h)));
        player = -1;
      }
      :: get_list t

let territories_from_file (json : Yojson.Basic.t) : territory list =
  let m = json |> Yojson.Basic.Util.to_list in
  get_list m

let get_player_number (t : territory) : int = t.player

let get_territory_from_string (name : string) (terr_list : territory list) :
    territory =
  try List.find (fun x -> String.compare x.name name = 0) terr_list
  with Not_found -> raise (UnknownTerritory name)

let rec territories_list (g : territory list) : string list =
  match g with
  | [] -> []
  | h :: t -> h.name :: territories_list t

let add_armies_to_territory (t : territory) (num : int) : territory =
  { t with num_troops = t.num_troops + num }

let set_territory_owner (t : territory) (player_num : int) : territory =
  { t with player = player_num }

let rec get_territories_from_continent (t : territory list) (s : string) :
    territory list =
  match t with
  | [] -> []
  | h :: w ->
      if String.compare h.continent s = 0 then
        h :: get_territories_from_continent w s
      else get_territories_from_continent w s

let rec num_territories (return : int) (t : territory list) : int =
  match t with
  | [] -> return
  | h :: t -> num_territories (return + 1) t

let get_neighbors (t : territory) : string list = t.neighbors
let get_territory_name (t : territory) : string = t.name
let get_territory_numtroops (t : territory) : int = t.num_troops
