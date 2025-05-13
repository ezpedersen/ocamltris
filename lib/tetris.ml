include Geometry

type t = {
  mutable score : int;
  mutable lines : int;
  well : string array array;
  rows : int;
  cols : int;
  mutable held : piece_type option;
  mutable piece : piece;
  future_pieces : piece_type Queue.t;
  mutable switched : bool;
  mutable game_over : bool;
  mutable bot_mode : bool ref;
  mutable last_bot_move : float;
  mutable bot_difficulty : int;
      (*0 to 4 for easy, medium, hard, expert, impossible*)
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
  List.for_all
    (fun { x; y } ->
      x >= 0 && x < g.cols && y >= 0 && y < g.rows && space_open g (x, y))
    (piece_pos p)

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
  g.score <- g.score + (num_cleared * num_cleared);
  g.lines <- g.lines + num_cleared

let get_score g = g.score

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
  let pt = Queue.take g.future_pieces in
  clear_lines g;
  Queue.add (random_piece_type ()) g.future_pieces;
  g.piece <- create_piece pt g.cols;
  if not @@ ok_place g.piece g then g.game_over <- true

let next_pos p x y =
  { p with position = { fx = p.position.fx +. x; fy = p.position.fy +. y } }

let shift g n =
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
  for i = 0 to 4 do
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

let hard_drop g =
  g.piece <- calculate_shadow g;
  add_to_well g

let is_game_over g = g.game_over

let get_garbage_row g () =
  let garbage_index = Random.int g.cols in
  let garbage_row =
    Array.init g.cols (fun i ->
        if i = garbage_index then "empty" else "garbage")
  in
  garbage_row

let tick g =
  let np = next_pos g.piece 0. 1. in
  if ok_place np g then
    let () = g.piece <- np in
    false
  else
    let () = add_to_well g in
    true

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

let create (cols, rows) bot_enabled difficulty =
  let well = Array.init rows (fun _ -> Array.init cols (fun _ -> "empty")) in
  let pt = random_piece_type () in
  let piece = create_piece pt cols in
  let future_pieces = Queue.create () in
  for _ = 1 to 5 do
    Queue.add (random_piece_type ()) future_pieces
  done;
  let game =
    {
      score = 0;
      lines = 0;
      well;
      cols;
      rows;
      piece;
      held = None;
      switched = false;
      future_pieces;
      game_over = false;
      bot_mode = ref bot_enabled;
      last_bot_move = Unix.gettimeofday ();
      bot_difficulty = difficulty;
    }
  in
  game

let hold g =
  if not g.switched then (
    let temp = g.piece.piece_type in
    if g.held = None then (
      let pt = Queue.take g.future_pieces in
      Queue.add (random_piece_type ()) g.future_pieces;
      g.piece <- create_piece pt g.cols)
    else g.piece <- create_piece (Option.get g.held) g.cols;
    g.held <- Some temp;
    g.switched <- true)

let add_garbage g n =
  if n = 0 then ()
  else
    for i = 0 to min (n - 1) (g.rows - 1) do
      if Array.exists (fun x -> x <> "empty") g.well.(i) then
        g.game_over <- true
    done;
  for i = n to g.rows - 1 do
    g.well.(i - n) <- g.well.(i)
  done;
  for i = max 0 (g.rows - n) to g.rows - 1 do
    g.well.(i) <- get_garbage_row g ()
  done;
  let i = ref n in
  while !i > 0 && not (ok_place g.piece g) do
    g.piece <- next_pos g.piece 0. (-1.);
    i := !i - 1
  done;
  if not (ok_place g.piece g) then g.game_over <- true

let get_cooldown difficulty =
  match difficulty with
  | 0 -> 0.4
  | 1 -> 0.2
  | 2 -> 0.1
  | 3 -> 0.05
  | _ -> 0.

let reset g =
  (* Reset the score to 0 *)
  g.score <- 0;

  (* Reset the well to empty *)
  for i = 0 to g.rows - 1 do
    g.well.(i) <- Array.make g.cols "empty"
  done;

  (* Set the game over flag to false *)
  g.game_over <- false;

  (* Create a new piece and reset the held piece *)
  let pt = random_piece_type () in
  g.piece <- create_piece pt g.cols;

  (* Clear any previously held piece *)
  g.held <- None;

  (* Clear and refill the future pieces queue *)
  Queue.clear g.future_pieces;
  for _ = 1 to 5 do
    Queue.add (random_piece_type ()) g.future_pieces
  done;

  (* Reset the switched flag *)
  g.switched <- false

let apply_bot_move g =
  if
    !(g.bot_mode)
    && Unix.gettimeofday () -. g.last_bot_move > get_cooldown g.bot_difficulty
  then begin
    let held_piece =
      match g.held with
      | None ->
          let made_piece = create_piece O g.cols in
          { made_piece with position = { made_piece.position with fy = 3. } }
      | Some piece_type ->
          let made_piece = create_piece piece_type g.cols in
          { made_piece with position = { made_piece.position with fy = 3. } }
    in
    let move = Bot.get_next_move g.well g.piece held_piece in
    (match move with
    | ShiftLeft -> shift g (-1)
    | ShiftRight -> shift g 1
    | RotateCW -> rotate_cw g
    | RotateCCW -> rotate_ccw g
    | Hold -> hold g
    | HardDrop -> hard_drop g);
    g.last_bot_move <- Unix.gettimeofday ();
    true
  end
  else false

let get_lines_cleared g = g.lines
let get_piece_position g = (g.piece.position.fx, g.piece.position.fy)
let get_piece_rotation g = !(g.piece.rotation)
let get_well g = g.well
