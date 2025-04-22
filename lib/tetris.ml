type point = {
  x : int;
  y : int;
}
(** respresents a point in 2D space *)

(** helper function for creating points *)
let point x y = { x; y }

type fpoint = {
  fx : float;
  fy : float;
}

let fpoint fx fy = { fx; fy }

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
  rotation : int ref;
  position : fpoint;
      (* position of "fpoint" of piece, see https://shorturl.at/GEqmK *)
}
(** represents a piece being controlled (one in the well its just blocks) *)

(*https://gamedev.stackexchange.com/questions/208367/how-is-rotation-defined-in-a-tetris-game*)

(** given a piece type, [piece_geometry t] is the points a piece takes up
    relative to its position *)
let base_geometry = function
  | I ->
      [ fpoint 0.5 0.5; fpoint 1.5 0.5; fpoint (-1.5) 0.5; fpoint (-0.5) 0.5 ]
  | J -> [ fpoint 0. 0.; fpoint 1. 0.; fpoint (-1.) 0.; fpoint (-1.) 1. ]
  | L -> [ fpoint 0. 0.; fpoint 1. 0.; fpoint (-1.) 0.; fpoint (-1.) 1. ]
  | O ->
      [
        fpoint 0.5 0.5;
        fpoint 0.5 (-0.5);
        fpoint (-0.5) 0.5;
        fpoint (-0.5) (-0.5);
      ]
  | S -> [ fpoint 0. 0.; fpoint 0. 1.; fpoint 1. 1.; fpoint 0. (-1.) ]
  | T -> [ fpoint 0. 0.; fpoint 0. 1.; fpoint 1. 0.; fpoint (-1.) 0. ]
  | Z -> [ fpoint 0. 0.; fpoint 0. 1.; fpoint (-1.) 1.; fpoint 1. 0. ]

let rotate_point_90 offset = fpoint offset.fy (-.offset.fx)

let rotated_geometry p =
  let rec rotation_function n x =
    if n = 0 then x else rotate_point_90 x |> rotation_function (n - 1)
  in
  List.map (rotation_function !(p.rotation)) (base_geometry p.piece_type)

let point_of_fpoint a = point (int_of_float a.fx) (int_of_float a.fy)

let piece_geometry p =
  List.map
    (fun x ->
      { fx = p.position.fx +. x.fx; fy = p.position.fy +. x.fy }
      |> point_of_fpoint)
    (rotated_geometry p)

let piece_pos = piece_geometry

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
      rotation = ref 0;
      position = { fx = 2.; fy = 0. };
    }
  in
  { score = 0; well; cols; rows; piece }

(** [true] if a block can be placed in [g]'s well at [(x, y)] *)
let space_open g (x, y) =
  if x >= g.cols || x < 0 || y >= g.rows || y < 0 then false
  else not g.well.(y).(x)

(** [collides p g] is true if [p] has blocks overlapping with [g]'s well or
    boundaries *)
let collides (p : piece) g =
  List.for_all (fun { x; y } -> space_open g (x, y)) (piece_pos p)

(** adds controlled piece to well and gives a new piece *)
let add_to_well g =
  List.iter (fun { x; y } -> g.well.(y).(x) <- true) (piece_pos g.piece);
  g.piece <-
    {
      piece_type = random_piece_type ();
      rotation = ref 0;
      position = { fx = float_of_int g.cols /. 2.; fy = 0. };
    }

let tick g =
  let next_pos =
    {
      g.piece with
      position = { g.piece.position with fy = g.piece.position.fy +. 1. };
    }
  in
  if collides next_pos g then g.piece <- next_pos else add_to_well g

let shift_right g n =
  let next_pos =
    {
      g.piece with
      position =
        { g.piece.position with fx = g.piece.position.fx +. float_of_int n };
    }
  in
  if collides next_pos g then g.piece <- next_pos

let rotate_cw g = g.piece.rotation := (!(g.piece.rotation) + 1) mod 4

let rotate_ccw g =
  g.piece.rotation :=
    let new_rot = !(g.piece.rotation) - 1 in
    if new_rot < 0 then 3 else new_rot

let get_entry g (x, y) =
  (not @@ space_open g (x, y))
  || List.exists
       (fun { x = p_x; y = p_y } -> x = p_x && y = p_y)
       (piece_pos g.piece)
