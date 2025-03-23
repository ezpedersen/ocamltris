type point = {
  x : int;
  y : int;
}
(** respresents a point in 2D space *)

(** helper function for creating points *)
let point x y = { x; y }

(** degrees rotated clockwise *)
type rotation =
  | R0
  | R90
  | R180
  | R270

(** Tetris pieces *)
type piece_type =
  | O
  | I
  | S
  | Z
  | L
  | J
  | T

type piece = {
  piece_type : piece_type;
  rotation : rotation;
  position : point;
      (* position of "top left" of piece, see https://shorturl.at/GEqmK *)
}
(** represents a piece being controlled (one in the well its just blocks) *)

(** given a piece type, [piece_geometry t] is the points a piece takes up
    relative to its position *)
let piece_geometry = function
  | O -> [ point 0 0; point 1 0; point 0 1; point 1 1 ]
  | I -> [ point 0 1; point 1 1; point 2 1; point 3 1 ]
  | S -> [ point 1 0; point 2 0; point 0 1; point 1 1 ]
  | Z -> [ point 0 0; point 1 0; point 1 1; point 2 1 ]
  | L -> [ point 2 0; point 0 1; point 1 1; point 2 1 ]
  | J -> [ point 0 0; point 0 1; point 1 1; point 2 1 ]
  | T -> [ point 1 0; point 0 1; point 1 1; point 2 1 ]

let piece_pos p =
  let unrotated = piece_geometry p.piece_type in
  let piece_width =
    match p.piece_type with
    | O -> 2
    | I -> 4
    | _ -> 3
  in
  let p_x, p_y = (p.position.x, p.position.y) in
  match p.rotation with
  | R0 -> List.map (fun { x; y } -> point (x + p_x) (y + p_y)) unrotated
  | R90 ->
      List.map
        (fun { x; y } -> point (piece_width - x + p_x) (y + p_y))
        unrotated
  | R180 ->
      List.map
        (fun { x; y } -> point (piece_width - x + p_x) (piece_width - y + p_y))
        unrotated
  | R270 ->
      List.map
        (fun { x; y } -> point (x + p_x) (piece_width - y + p_y))
        unrotated

type t = {
  score : int;
  well : bool array array;
      (* TODO: change to color or something so it looks better*)
  rows : int;
  cols : int;
  mutable piece : piece;
}

let random_piece_type () =
  let choice = Random.int 7 in
  match choice with
  | 0 -> O
  | 1 -> I
  | 2 -> S
  | 3 -> Z
  | 4 -> L
  | 5 -> J
  | 6 -> T
  | _ -> failwith "unreachable"

let create (cols, rows) =
  let well = Array.init rows (fun _ -> Array.init cols (fun _ -> false)) in
  let piece =
    {
      piece_type = random_piece_type ();
      rotation = R0;
      position = { x = cols / 2; y = 0 };
    }
  in
  { score = 0; well; cols; rows; piece }

(** [true] if a block can be placed in [g]'s well at [(x, y)] *)
let space_open g (x, y) =
  if x >= g.cols || x < 0 || y >= g.rows || y < 0 then false
  else not g.well.(y).(x)

(** [collides p g] is true if [p] has blocks overlapping with [g]'s well or
    boundaries *)
let collides p g =
  List.for_all (fun { x; y } -> space_open g (x, y)) (piece_pos p)

(** adds controlled piece to well and gives a new piece *)
let add_to_well g =
  List.iter (fun { x; y } -> g.well.(y).(x) <- true) (piece_pos g.piece);
  g.piece <-
    {
      piece_type = random_piece_type ();
      rotation = R0;
      position = { x = g.cols / 2; y = 0 };
    }

let tick g =
  let next_pos =
    {
      g.piece with
      position = { g.piece.position with y = g.piece.position.y + 1 };
    }
  in
  if collides next_pos g then g.piece <- next_pos else add_to_well g

let shift_right g n =
  let next_pos =
    {
      g.piece with
      position = { g.piece.position with x = g.piece.position.x + n };
    }
  in
  if collides next_pos g then g.piece <- next_pos

let get_entry g (x, y) =
  (not @@ space_open g (x, y))
  || List.exists
       (fun { x = p_x; y = p_y } -> x = p_x && y = p_y)
       (piece_pos g.piece)
