include Geometry

(** the various inputs that a bot can give *)
type move =
  | ShiftLeft
  | ShiftRight
  | RotateCW
  | RotateCCW
  | HardDrop
  | Hold

(** potential placement/move with piece*)
type placement = {
  position : float * float; (* x, y coordinates *)
  rotation : int; (* 0-3 for rotation state *)
  score : float; (* Heuristic score *)
  lines_cleared : int; (* Number of lines cleared *)
}

(** constant values dictating bot behavior *)
type constants = {
  aggregate_height : float;
  complete_lines : float;
  holes : float;
  bumpiness : float;
}

(** settings instances of the bot use *)
let weights =
  {
    aggregate_height = -0.510066;
    complete_lines = 0.760666;
    holes = -0.35663;
    bumpiness = -0.184483;
  }

(** gets the points of piece with [rotation_value] at location [(x, y)]*)
let get_piece_points piece_type (rotation_value : int) (x, y) =
  piece_pos (make_piece piece_type (ref rotation_value) (fpoint x y))

(** returns whether or not a piece can be placed in the given scenario *)
let can_place_piece well piece_type rotation (pos_x, pos_y) aggressive_mode =
  let rows = Array.length well in
  let cols = if rows > 0 then Array.length well.(0) else 0 in
  let right_side = if aggressive_mode then cols - 1 else cols in
  let piece_points = get_piece_points piece_type rotation (pos_x, pos_y) in
  List.for_all
    (fun { x; y } ->
      x >= 0 && x < right_side && y >= 0 && y < rows && well.(y).(x) = "empty")
    piece_points

(** places piece into well at provided position *)
let place_piece well piece_type rotation (pos_x, pos_y) =
  let new_well = Array.map Array.copy well in
  let piece_points = get_piece_points piece_type rotation (pos_x, pos_y) in
  List.iter
    (fun { x; y } ->
      if
        y >= 0
        && y < Array.length new_well
        && x >= 0
        && x < Array.length new_well.(0)
      then new_well.(y).(x) <- string_of_piece_type piece_type)
    piece_points;

  new_well

(** gets the heights of columns in a well *)
let get_column_heights well =
  let rows = Array.length well in
  let cols = if rows > 0 then Array.length well.(0) else 0 in
  let heights = Array.make cols 0 in

  for col = 0 to cols - 1 do
    let rec find_height row =
      if row >= rows then rows
      else if well.(row).(col) <> "empty" then row
      else find_height (row + 1)
    in
    heights.(col) <- rows - find_height 0
  done;

  heights

(** returns the sum of the provided column heights *)
let aggregate_height column_heights = Array.fold_left ( + ) 0 column_heights

(** counts the number of complete lines in provided well *)
let count_complete_lines well =
  let rows = Array.length well in
  let cols = if rows > 0 then Array.length well.(0) else 0 in
  let count = ref 0 in

  for row = 0 to rows - 1 do
    let complete = ref true in
    for col = 0 to cols - 1 do
      if well.(row).(col) = "empty" then complete := false
    done;
    if !complete then count := !count + 1
  done;

  !count

(** returns the number of complete lines in well, with adjusted iteration*)
let adjusted_complete_lines well =
  let rows = Array.length well in
  let cols = if rows > 0 then Array.length well.(0) else 0 in
  let count = ref 0 in

  for row = 0 to rows - 1 do
    let complete = ref true in
    for col = 0 to cols - 2 do
      if well.(row).(col) = "empty" then complete := false
    done;
    if !complete then count := !count + 1
  done;

  !count

(** counts holes in given well *)
let count_holes well =
  let rows = Array.length well in
  let cols = if rows > 0 then Array.length well.(0) else 0 in
  let count = ref 0 in

  for col = 0 to cols - 1 do
    for row = 1 to rows - 1 do
      if well.(row).(col) == "empty" && well.(row - 1).(col) <> "empty" then
        count := !count + 1
      else ()
    done
  done;

  !count

(** returns the bumpiness of the provided column heights*)
let bumpiness column_heights =
  let sum = ref 0 in

  for i = 0 to Array.length column_heights - 2 do
    sum := !sum + abs (column_heights.(i) - column_heights.(i + 1))
  done;

  !sum

(** returns bumpiness of provided column heights with adjusted iteration *)
let adjusted_bumpiness column_heights =
  if Array.length column_heights <= 1 then 0.0
    (* Return 0 if there are no columns or just one column *)
  else
    let sum = ref 0. in

    (* Iterate through all pairs of adjacent columns *)
    for i = 0 to Array.length column_heights - 2 do
      sum :=
        !sum
        +. abs_float
             (float_of_int column_heights.(i)
             -. float_of_int column_heights.(i + 1))
    done;

    !sum

(** evaluate the current game state *)
let evaluate_state well aggressive_mode =
  let column_heights = get_column_heights well in
  let agg_height = aggregate_height column_heights in
  let complete = count_complete_lines well in
  let holes = count_holes well in
  let bumps = bumpiness column_heights in

  let adjusted = adjusted_bumpiness column_heights in
  let adjusted_complete = adjusted_complete_lines well in

  (*default line clearing strategy*)
  if not aggressive_mode then
    (weights.aggregate_height *. float_of_int agg_height)
    +. (weights.complete_lines *. float_of_int complete)
    +. (weights.holes *. float_of_int holes)
    +. (weights.bumpiness *. float_of_int bumps)
    (*otherwise go aggressive with a right side stack, build without I, clear
      with I*)
  else
    (weights.aggregate_height *. float_of_int agg_height)
    +. (0. *. weights.complete_lines *. float_of_int adjusted_complete)
    +. (2.5 *. weights.holes *. float_of_int holes)
    +. (weights.bumpiness *. adjusted)

(* (weights.aggregate_height *. float_of_int agg_height) +.
   (weights.complete_lines *. float_of_int complete) +. (weights.holes *.
   float_of_int holes) +. (weights.bumpiness *. float_of_int bumps) *)

(** drops the provided piece *)
let drop_piece well piece_type rotation (x, y) =
  let rec drop y =
    if can_place_piece well piece_type rotation (x, y +. 1.) false then
      drop (y +. 1.)
    else y
  in
  drop y

(** finds all placements of the provided piece in the provided well *)
let find_all_placements well piece =
  let column_heights = get_column_heights well in
  let aggressive_mode =
    (piece.piece_type <> I
    || column_heights.(Array.length column_heights - 2)
       - column_heights.(Array.length column_heights - 1)
       < 2)
    && aggregate_height column_heights < 55
  in
  let rows = Array.length well in
  let cols = if rows > 0 then Array.length well.(0) else 0 in
  let placements = ref [] in

  let max_rotations = 4 in
  for rot = 0 to max_rotations - 1 do
    for x = -3 to cols + 3 do
      let convert x = float_of_int x in
      if
        can_place_piece well piece.piece_type rot
          (convert x, 3.0)
          aggressive_mode
      then
        let final_y = drop_piece well piece.piece_type rot (convert x, 3.0) in
        let new_well =
          place_piece well piece.piece_type rot (convert x, final_y)
        in
        let score = evaluate_state new_well aggressive_mode in
        let lines_cleared = count_complete_lines new_well in
        placements :=
          {
            position = (convert x, final_y);
            rotation = rot;
            score;
            lines_cleared;
          }
          :: !placements
      else ()
    done
  done;

  !placements

(** finds the best placement of the given piece in the given well *)
let find_best_placement well piece =
  let placements = find_all_placements well piece in

  match placements with
  | [] -> None
  | _ ->
      let best, score, lines_cleared =
        List.fold_left
          (fun (best, score, lines_cleared) p ->
            if p.score > best.score then (p, p.score, p.lines_cleared)
            else (best, score, lines_cleared))
          ( List.hd placements,
            (List.hd placements).score,
            (List.hd placements).lines_cleared )
          placements
      in
      Some (best, score, lines_cleared)

(** returns whether the bot should hold the piece in the given scenario *)
let should_hold well piece held_piece =
  if piece.piece_type = held_piece.piece_type then false
  else
    match find_best_placement well piece with
    | None -> false
    | Some (_, piece_score, lines_cleared) -> (
        match find_best_placement well held_piece with
        | None -> true
        | Some (_, held_score, held_lines_cleared) ->
            if held_score > piece_score then true else false)

(** returns a sequence of moves for the given piece *)
let generate_move_sequence_with_piece (piece : piece) target =
  let moves = ref [] in

  let current_rot = !(piece.rotation) in
  let target_rot = target.rotation in
  let rot_diff = (target_rot - current_rot + 4) mod 4 in

  if rot_diff = 1 then moves := RotateCCW :: !moves
  else if rot_diff = 2 then moves := RotateCCW :: RotateCCW :: !moves
  else if rot_diff = 3 then moves := RotateCW :: !moves;

  let target_x = fst target.position in
  let current_x = piece.position.fx in
  let shift_diff = int_of_float (target_x -. current_x) in

  if shift_diff < 0 then
    for _ = 1 to abs shift_diff do
      moves := ShiftLeft :: !moves
    done
  else
    for _ = 1 to shift_diff do
      moves := ShiftRight :: !moves
    done;
  moves := HardDrop :: !moves;

  List.rev !moves

(** generates a move sequence for the given gamestate *)
let generate_move_sequence well piece held_piece target held_target =
  if should_hold well piece held_piece then
    let moves = generate_move_sequence_with_piece held_piece held_target in
    Hold :: moves
  else
    let moves = generate_move_sequence_with_piece piece target in
    moves

(** gets the next move in the provided gamestate *)
let get_next_move well piece held_piece =
  match find_best_placement well piece with
  | None -> HardDrop
  | Some (target, _, _) -> (
      match find_best_placement well held_piece with
      | None -> HardDrop
      | Some (held_target, _, _) -> (
          match
            generate_move_sequence well piece held_piece target held_target
          with
          | [] -> HardDrop
          | move :: tail -> move))
          
(** geets the next move sequence in the provided gamestate *)
let get_next_move_sequence well piece held_piece =
  match find_best_placement well piece with
  | None -> [ HardDrop ]
  | Some (target, _, _) -> (
      match find_best_placement well held_piece with
      | None -> [ HardDrop ]
      | Some (held_target, _, _) ->
          generate_move_sequence well piece held_piece target held_target)
