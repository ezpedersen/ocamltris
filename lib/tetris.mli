(** a tetris game *)
type t
(** returns a tetris game on an [x] by [y] board *)
val create : int * int -> t
(** causes selected block to fall down one tile *)
val tick : t -> bool
(** shifts the selected block [n] units right and returns true if block was placed *)
val shift_right : t -> int -> unit
(** [get_entry b (x, y)] is true if a block exists at [(x, y)] in [b] *)
val get_entry : t -> int * int -> int
(** hards drops the piece *)
val hard_drop : t -> unit

(** rotates the piece counterclockwise *)
val rotate_ccw : t -> unit
(** rotates the piece clockwise *)
val rotate_cw : t -> unit

(** returns score *)
val get_score : t -> int
(** returns held piece as string *)
val get_held : t -> string
(** holds a piece *)
val hold : t -> unit
