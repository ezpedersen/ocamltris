type t
(** a 2-Player tetris game *)

val create : int * int -> t
(** creates a 2 player tetris game on [x] by [y] boards *)

val tick_left : t -> unit
(** causes selected block to fall down one tile on left player's board*)
val tick_right : t -> unit
(** causes selected block to fall down one tile on right player's board*)
val shift_left : t -> int -> unit
(** shifts the selected block [n] units right on left player's board*)
val shift_right : t -> int -> unit
(** shifts the selected block [n] units right on right player's board*)

val get_entry_left : t -> int * int -> string
(** [get_entry b (x, y)] is true if a block exists at [(x, y)] in [b] on left player's board*)
val get_entry_right : t -> int * int -> string
(** [get_entry b (x, y)] is true if a block exists at [(x, y)] in [b] on right player's board*)

val rotate_ccw_left : t -> unit
(** rotates the piece counterclockwise on left player's board*)
val rotate_ccw_right : t -> unit
(** rotates the piece counterclockwise on right player's board*)
val rotate_cw_left : t -> unit
(** rotates the piece clockwise on left player's board*)
val rotate_cw_right : t -> unit
(** rotates the piece clockwise on right player's board*)
val hard_drop_left : t -> unit
(** hards drops the piece on left player's board*)
val hard_drop_right : t -> unit
(** hards drops the piece on right player's board*)

val get_scores : t -> int * int
(** returns scores of left and right players *)
val get_held : t -> string * string
(** returns held pieces of left and right players *)
val hold_left : t -> unit
(** holds a piece on left player's board *)
val hold_right : t -> unit
(** holds a piece on right player's board *)
val is_game_over : t -> bool * bool
(** checks if player's games are game-over*)

val apply_bot_move: t -> unit
(** Advances the bots on both boards, if they are enabled.*)