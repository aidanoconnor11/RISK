(**TODO: Finish Documentation w/ more than bare bones*)
type territory
(**The abstract type representing a territory*)

exception UnknownTerritory of string
(**Raised when asked for a territory that doesn't exist*)

exception NotNeighbors of string
(**Raised when a player attempts to attack a non-neighbor*)

val territories_from_file : Yojson.Basic.t -> territory list
(**Gets files from specified file, puts them in a territory list*)

val get_territory_from_string : string -> territory list -> territory
(**Returns territory of the name specified*)

val add_armies_to_territory : territory -> int -> territory

val get_territories_from_continent : territory list -> string -> territory list
(**Returns the territories from a given continent*)

val num_territories : int -> territory list -> int
val set_territory_owner : territory -> int -> territory
val get_player_number : territory -> int
val num_troops : territory -> int
val territories_list : territory list -> string list
val get_territory_name : territory -> string
val get_territory_numtroops : territory -> int
val get_neighbors : territory -> string list
