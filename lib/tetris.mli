(** a tetris game *)
type t

(** [create x y bot diff]returns a tetris game on an [x] by [y] board, with 
[bot] determining if a bot is playing and [diff] determing the difficulty. 
  Requires: 0 <= [diff] <= 4.*)
val create : int * int -> bool -> int -> t
(** causes selected block to fall down one tile *)
val tick : t -> bool
(** shifts the selected block [n] units right and returns true if block was placed *)
val shift : t -> int -> unit
(** [get_entry b (x, y)] is true if a block exists at [(x, y)] in [b] *)
val get_entry : t -> int * int -> string
(** hards drops the piece *)
val hard_drop : t -> unit

(** rotates the piece counterclockwise *)
val rotate_ccw : t -> unit
(** rotates the piece clockwise *)
val rotate_cw : t -> unit

(** returns score *)
val get_score : t -> int
(** returns the number of lines cleared *)
val get_lines_cleared : t -> int
(** returns held piece as string *)
val get_held : t -> string
(** holds a piece *)
val hold : t -> unit
(** checks if game is game-over*)
val is_game_over : t -> bool
(** adds a [n] lines of garbage to the bottom of the board *)
val add_garbage : t -> int -> unit
(** clears the board and resets the score *)
val reset : t -> unit
(**Applies the AI bot to move once on the given gameboard if enabled. 
  Returns a boolean on whether or not the move applied.*)
val apply_bot_move : t -> bool

(** Added for testing purposes *)
val get_piece_position : t -> float * float
(** Added for testing purposes *)
val get_piece_rotation : t -> int
(** Added for testing purposes *)
val get_well : t -> string array array
