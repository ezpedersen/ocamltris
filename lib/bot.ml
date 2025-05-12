include Geometry

type move =
  | ShiftLeft
  | ShiftRight
  | RotateCW
  | RotateCCW
  | HardDrop
  | Hold

(*Potential placement/move with piece*)
type placement = {
  position : float * float; (* x, y coordinates *)
  rotation : int; (* 0-3 for rotation state *)
  score : float; (* Heuristic score *)
}

type constants = {
  aggregate_height : float;
  complete_lines : float;
  holes : float;
  bumpiness : float;
}

let weights =
  {
    aggregate_height = -0.510066;
    complete_lines = 0.760666;
    holes = -0.35663;
    bumpiness = -0.184483;
  }

let get_piece_points piece_type (rotation_value : int) (x, y) =
  piece_pos (make_piece piece_type (ref rotation_value) (fpoint x y))

let can_place_piece well piece_type rotation (pos_x, pos_y) =
  let rows = Array.length well in
  let cols = if rows > 0 then Array.length well.(0) else 0 in
  let piece_points = get_piece_points piece_type rotation (pos_x, pos_y) in
  List.for_all
    (fun { x; y } ->
      x >= 0 && x < cols && y >= 0 && y < rows && well.(y).(x) = "empty")
    piece_points

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

let aggregate_height column_heights = Array.fold_left ( + ) 0 column_heights

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

let bumpiness column_heights =
  let sum = ref 0 in

  for i = 0 to Array.length column_heights - 2 do
    sum := !sum + abs (column_heights.(i) - column_heights.(i + 1))
  done;

  !sum

let evaluate_state well =
  let column_heights = get_column_heights well in
  let agg_height = aggregate_height column_heights in
  let complete = count_complete_lines well in
  let holes = count_holes well in
  let bumps = bumpiness column_heights in

  (weights.aggregate_height *. float_of_int agg_height)
  +. (weights.complete_lines *. float_of_int complete)
  +. (weights.holes *. float_of_int holes)
  +. (weights.bumpiness *. float_of_int bumps)

let drop_piece well piece_type rotation (x, y) =
  let rec drop y =
    if can_place_piece well piece_type rotation (x, y +. 1.) then drop (y +. 1.)
    else y
  in
  drop y

let find_all_placements well piece =
  let rows = Array.length well in
  let cols = if rows > 0 then Array.length well.(0) else 0 in
  let placements = ref [] in

  let max_rotations = 4 in

  for rot = 0 to max_rotations - 1 do
    for x = -3 to cols + 3 do
      let convert x = float_of_int x in
      if can_place_piece well piece.piece_type rot (convert x, 3.0) then
        let final_y = drop_piece well piece.piece_type rot (convert x, 3.0) in
        let new_well =
          place_piece well piece.piece_type rot (convert x, final_y)
        in
        let score = evaluate_state new_well in
        placements :=
          { position = (convert x, final_y); rotation = rot; score }
          :: !placements
      else ()
    done
  done;

  !placements

let find_best_placement well piece =
  let placements = find_all_placements well piece in

  match placements with
  | [] -> None
  | _ ->
      let best, score =
        List.fold_left
          (fun (best, score) p ->
            if p.score > best.score then (p, p.score) else (best, score))
          (List.hd placements, (List.hd placements).score)
          placements
      in
      Some (best, score)

let should_hold well piece held_piece =
  match find_best_placement well piece with
  | None -> false
  | Some (_, piece_score) -> (
      match find_best_placement well held_piece with
      | None -> true
      | Some (_, held_score) -> if held_score > piece_score then true else false
      )

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

let generate_move_sequence well piece held_piece target held_target =
  if should_hold well piece held_piece then
    let moves = generate_move_sequence_with_piece held_piece held_target in
    Hold :: moves
  else
    let moves = generate_move_sequence_with_piece piece target in
    moves

let get_next_move well piece held_piece =
  match find_best_placement well piece with
  | None -> HardDrop
  | Some (target, _) -> (
      match find_best_placement well held_piece with
      | None -> HardDrop
      | Some (held_target, _) -> (
          match
            generate_move_sequence well piece held_piece target held_target
          with
          | [] -> HardDrop
          | move :: tail -> move))

let get_next_move_sequence well piece held_piece =
  match find_best_placement well piece with
  | None -> [ HardDrop ]
  | Some (target, _) -> (
      match find_best_placement well held_piece with
      | None -> [ HardDrop ]
      | Some (held_target, _) ->
          generate_move_sequence well piece held_piece target held_target)
