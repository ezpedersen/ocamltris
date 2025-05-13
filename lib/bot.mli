(** Tetris AI bot implementing a heuristic-based approach taken from
    https://codemyroad.wordpress.com/2013/04/14/tetris-ai-the-near-perfect-player/*)

(** The possible moves the AI can make *)
type move =
  | ShiftLeft
  | ShiftRight
  | RotateCW
  | RotateCCW
  | HardDrop
  | Hold

type piece = Geometry.piece
(** The type of a tetris piece. *)

val get_next_move : string array array -> piece -> piece -> move
(** Gets the next best move for the AI based on well state and piece info *)

val get_next_move_sequence : string array array -> piece -> piece -> move list
(** Gets the sequence of optimal moves for a given piece, from start to hard
    dropping. *)

val evaluate_state : string array array -> bool -> float
(** Added for testing purposes *)
