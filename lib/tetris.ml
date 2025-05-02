type point = {
  x : int;
  y : int;
}
(* respresents a point in 2D space *)

(* helper function for creating points *)
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
(* represents a piece being controlled (one in the well its just blocks) *)

(*https://gamedev.stackexchange.com/questions/208367/how-is-rotation-defined-in-a-tetris-game*)

(* given a piece type, [piece_geometry t] is the points a piece takes up
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

let point_of_fpoint a =
  point (int_of_float (floor a.fx)) (a.fy |> floor |> int_of_float)

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

(* [true] if a block can be placed in [g]'s well at [(x, y)] *)
let space_open g (x, y) =
  if x >= g.cols || x < 0 || y >= g.rows || y < 0 then false
  else not g.well.(y).(x)

(* [ok_place p g] is true if [p] has blocks overlapping with [g]'s well or
   boundaries *)
let ok_place (p : piece) g =
  List.for_all (fun { x; y } -> space_open g (x, y)) (piece_pos p)

let clear_lines g =
  let row_full row = Array.for_all (fun x -> x) row in

  let new_rows =
    Array.fold_right
      (fun row acc -> if row_full row then acc else row :: acc)
      g.well []
  in
  let num_cleared = g.rows - List.length new_rows in
  for x = 0 to num_cleared - 1 do
    g.well.(x) <- Array.make g.cols false
  done;
  for x = 0 to List.length new_rows - 1 do
    g.well.(x + num_cleared) <- List.nth new_rows x
  done

(* adds controlled piece to well and gives a new piece *)
let add_to_well g =
  List.iter (fun { x; y } -> g.well.(y).(x) <- true) (piece_pos g.piece);
  g.piece <-
    {
      piece_type = random_piece_type ();
      rotation = ref 0;
      position = { fx = float_of_int g.cols /. 2.; fy = 0. };
    };
  clear_lines g

let next_pos p x y =
  { p with position = { fx = p.position.fx +. x; fy = p.position.fy +. y } }

let tick g =
  let np = next_pos g.piece 0. 1. in
  if ok_place np g then
    let () = g.piece <- np in
    false
  else
    let () = add_to_well g in
    true

let shift_right g n =
  let n = float_of_int n in
  let np = next_pos g.piece n 0. in
  if ok_place np g then g.piece <- np

let rotate g n =
  let old_rot = !(g.piece.rotation) in
  let new_rot = !(g.piece.rotation) + n in
  let new_rot = if new_rot < 0 then 3 else if new_rot = 4 then 0 else new_rot in
  g.piece.rotation := new_rot;
  if ok_place g.piece g then ()
  else
    let np = next_pos g.piece 1. 0. in
    if ok_place np g then g.piece <- np
    else
      let np = next_pos g.piece (-1.) 0. in
      if ok_place np g then g.piece <- np else g.piece.rotation := old_rot;
      ()

let rotate_ccw g = rotate g 1
let rotate_cw g = rotate g (-1)

let calculate_shadow g =
  let np = ref (next_pos g.piece 0. 1.) in
  let good = ref g.piece in
  while ok_place !np g do
    good := !np;
    np := next_pos !np 0. 1.
  done;
  !good

let get_entry g (x, y) =
  if
    (not @@ space_open g (x, y))
    || List.exists
         (fun { x = p_x; y = p_y } -> x = p_x && y = p_y)
         (piece_pos g.piece)
  then 1
  else if
    List.exists
      (fun { x = p_x; y = p_y } -> x = p_x && y = p_y)
      (piece_pos @@ calculate_shadow g)
  then 2
  else 0

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

let hard_drop g =
  g.piece <- calculate_shadow g;
  add_to_well g
