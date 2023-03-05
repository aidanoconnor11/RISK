module type Country = sig
  type country
end

module type Move = sig
  type move

  val player_moving : move -> string
  val army_type : move -> string (*Placeholder*)
  val country_attacking : move -> string
  val valid_move : move -> bool
end

module type Board = sig
  type boardstate

  val init_boardstate : int -> boardstate
  (*If we want more parameters for how to start the board, this is where they
    would be changed, int rep. num_player*)

  val advance_boardstate : Move.t -> boardstate -> boardstate
end
