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
  | Z -> [ fpoint 0. 0.; fpoint (-1.) 0.; fpoint 0. 1.; fpoint 1. 1. ]
  | I ->
      [ fpoint 0.5 0.5; fpoint 1.5 0.5; fpoint (-1.5) 0.5; fpoint (-0.5) 0.5 ]
  | J -> [ fpoint 0. 0.; fpoint 1. 0.; fpoint (-1.) 0.; fpoint 1. 1. ]
  | L -> [ fpoint 0. 0.; fpoint 1. 0.; fpoint (-1.) 0.; fpoint (-1.) 1. ]
  | O ->
      [
        fpoint 0.5 0.5;
        fpoint 0.5 (-0.5);
        fpoint (-0.5) 0.5;
        fpoint (-0.5) (-0.5);
      ]
  | T -> [ fpoint 0. 0.; fpoint 0. 1.; fpoint 1. 1.; fpoint (-1.) 1. ]
  | S -> [ fpoint 0. 0.; fpoint 0. 1.; fpoint (-1.) 1.; fpoint 1. 0. ]

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
  score : int ref;
  well : string array array;
      (* TODO: change to color or something so it looks better*)
  rows : int;
  cols : int;
  mutable held : piece_type option;
  mutable piece : piece;
  mutable switched : bool;
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
  else g.well.(y).(x) = "empty"

(* [ok_place p g] is true if [p] has blocks overlapping with [g]'s well or
   boundaries *)
let ok_place (p : piece) g =
  List.for_all (fun { x; y } -> space_open g (x, y)) (piece_pos p)

let clear_lines g =
  let row_full row = Array.for_all (fun x -> x <> "empty") row in

  let new_rows =
    Array.fold_right
      (fun row acc -> if row_full row then acc else row :: acc)
      g.well []
  in
  let num_cleared = g.rows - List.length new_rows in
  for x = 0 to num_cleared - 1 do
    g.well.(x) <- Array.make g.cols "empty"
  done;
  for x = 0 to List.length new_rows - 1 do
    g.well.(x + num_cleared) <- List.nth new_rows x
  done;
  g.score := !(g.score) + (num_cleared * num_cleared)

let get_score g = !(g.score)

let string_of_piece_type = function
  | O -> "O"
  | I -> "I"
  | S -> "S"
  | Z -> "Z"
  | L -> "L"
  | J -> "J"
  | T -> "T"

let get_held g =
  match g.held with
  | Some x -> string_of_piece_type x
  | None -> "None"

let create_piece pt cols =
  {
    piece_type = pt;
    rotation = ref 0;
    position =
      {
        fx = float_of_int cols /. 2.;
        fy = (if pt = O || pt = I then 1. else 0.);
      };
  }

(* [add_to_well g] adds the piece to the well and gives a new piece *)
(* adds controlled piece to well and gives a new piece *)
let add_to_well g =
  g.switched <- false;
  List.iter
    (fun { x; y } -> g.well.(y).(x) <- string_of_piece_type g.piece.piece_type)
    (piece_pos g.piece);
  let pt = random_piece_type () in
  clear_lines g;
  g.piece <- create_piece pt g.cols;
  if not @@ ok_place g.piece g then failwith "Game over!"

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

let wall_kicks_jltsz =
  let tbl = Hashtbl.create 8 in
  let add ft offsets = Hashtbl.add tbl ft offsets in
  add (0, 1) [ (0, 0); (-1, 0); (-1, 1); (0, -2); (-1, -2) ];
  add (1, 0) [ (0, 0); (1, 0); (1, -1); (0, 2); (1, 2) ];
  add (1, 2) [ (0, 0); (1, 0); (1, -1); (0, 2); (1, 2) ];
  add (2, 1) [ (0, 0); (-1, 0); (-1, 1); (0, -2); (-1, -2) ];
  add (2, 3) [ (0, 0); (1, 0); (1, 1); (0, -2); (1, -2) ];
  add (3, 2) [ (0, 0); (-1, 0); (-1, -1); (0, 2); (-1, 2) ];
  add (3, 0) [ (0, 0); (-1, 0); (-1, -1); (0, 2); (-1, 2) ];
  add (0, 3) [ (0, 0); (1, 0); (1, 1); (0, -2); (1, -2) ];
  tbl

let wall_kicks_i =
  let tbl = Hashtbl.create 8 in
  let add ft offsets = Hashtbl.add tbl ft offsets in
  add (0, 1) [ (0, 0); (-2, 0); (1, 0); (-2, -1); (1, 2) ];
  add (1, 0) [ (0, 0); (2, 0); (-1, 0); (2, 1); (-1, -2) ];
  add (1, 2) [ (0, 0); (-1, 0); (2, 0); (-1, 2); (2, -1) ];
  add (2, 1) [ (0, 0); (1, 0); (-2, 0); (1, -2); (-2, 1) ];
  add (2, 3) [ (0, 0); (2, 0); (-1, 0); (2, 1); (-1, -2) ];
  add (3, 2) [ (0, 0); (-2, 0); (1, 0); (-2, -1); (1, 2) ];
  add (3, 0) [ (0, 0); (1, 0); (-2, 0); (1, -2); (-2, 1) ];
  add (0, 3) [ (0, 0); (-1, 0); (2, 0); (-1, 2); (2, -1) ];
  tbl

(* https://tetris.fandom.com/wiki/SRS *)
let rotate g n =
  let old_rot = !(g.piece.rotation) in
  let new_rot = !(g.piece.rotation) + n in
  let new_rot = if new_rot < 0 then 3 else if new_rot = 4 then 0 else new_rot in
  g.piece.rotation := new_rot;
  let offset_lib =
    if g.piece.piece_type = I then wall_kicks_i else wall_kicks_jltsz
  in
  let offsets = Hashtbl.find offset_lib (old_rot, new_rot) in
  let good = ref false in
  for i = 0 to 7 do
    if not !good then
      let x, y = List.nth offsets i in
      let new_pos = next_pos g.piece (float_of_int x) (float_of_int y) in
      if ok_place new_pos g then (
        good := true;
        g.piece <- { g.piece with position = new_pos.position })
  done;
  if not !good then g.piece.rotation := old_rot

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
  if not @@ space_open g (x, y) then g.well.(y).(x)
  else if
    List.exists
      (fun { x = p_x; y = p_y } -> x = p_x && y = p_y)
      (piece_pos g.piece)
  then string_of_piece_type g.piece.piece_type
  else if
    List.exists
      (fun { x = p_x; y = p_y } -> x = p_x && y = p_y)
      (piece_pos @@ calculate_shadow g)
  then "shadow"
  else "empty"

let create (cols, rows) =
  let well = Array.init rows (fun _ -> Array.init cols (fun _ -> "empty")) in
  let pt = random_piece_type () in
  let piece = create_piece pt cols in
  { score = ref 0; well; cols; rows; piece; held = None; switched = false }

let hold g =
  if not g.switched then (
    let temp = g.piece.piece_type in
    if g.held = None then
      let pt = random_piece_type () in
      g.piece <- create_piece pt g.cols
    else g.piece <- create_piece (Option.get g.held) g.cols;
    g.held <- Some temp;
    g.switched <- true)

let hard_drop g =
  g.piece <- calculate_shadow g;
  add_to_well g
