(** a tetris game *)
type t
(** returns a tetris game on an [x] by [y] board *)
val create : int * int -> t
(** causes selected block to fall down one tile *)
val tick : t -> unit
(** shifts the selected block [n] units right *)
val shift_right : t -> int -> unit
(** [get_entry b (x, y)] is true if a block exists at [(x, y)] in [b] *)
val get_entry : t -> int * int -> bool
