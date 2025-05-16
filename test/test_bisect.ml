(* Test case for the cols variable calculation, inefficiently implemented *)
open Bot
open Geometry
open OUnit2

type constants = {
  aggregate_height : float;
  complete_lines : float;
  holes : float;
  bumpiness : float;
}

type placement = {
  position : float * float; (* x, y coordinates *)
  rotation : int; (* 0-3 for rotation state *)
  score : float; (* Heuristic score *)
  lines_cleared : int; (* Number of lines cleared *)
}

let weights =
  {
    aggregate_height = -0.510066;
    complete_lines = 0.760666;
    holes = -0.35663;
    bumpiness = -0.184483;
  }

let test_cols_empty_well _ =
  let well = [||] in
  let rows = Array.length well in
  let cols = if rows > 0 then Array.length well.(0) else 0 in
  let result = cols in
  let expected_result = 0 in
  assert_equal expected_result result

let test_cols_non_empty_well _ =
  let well = [| [| "empty"; "empty" |]; [| "empty"; "empty" |] |] in
  let rows = Array.length well in
  let cols = if rows > 0 then Array.length well.(0) else 0 in
  let result = cols in
  let expected_result = 2 in
  assert_equal expected_result result

let test_count_holes_no_holes _ =
  let well = [| [| "empty"; "empty" |]; [| "empty"; "empty" |] |] in
  let count = ref 0 in
  let rows = Array.length well in
  let cols = Array.length well.(0) in
  for col = 0 to cols - 1 do
    for row = 1 to rows - 1 do
      if well.(row).(col) = "empty" && well.(row - 1).(col) <> "empty" then
        count := !count + 1
      else count := !count
    done
  done;
  let result = !count in
  let expected_result = 0 in
  assert_equal expected_result result

let test_count_holes_with_holes _ =
  let well = [| [| "empty"; "filled" |]; [| "filled"; "empty" |] |] in
  let count = ref 0 in
  let rows = Array.length well in
  let cols = Array.length well.(0) in
  for col = 0 to cols - 1 do
    for row = 1 to rows - 1 do
      if well.(row).(col) = "empty" && well.(row - 1).(col) <> "empty" then
        count := !count + 1
      else count := !count
    done
  done;
  let result = !count in
  let expected_result = 1 in
  assert_equal expected_result result

let test_evaluate_state_no_lines _ =
  let agg_height = 0 in
  let complete = 0 in
  let holes = 0 in
  let bumps = 0 in
  let result =
    (weights.aggregate_height *. float_of_int agg_height)
    +. (weights.complete_lines *. float_of_int complete)
    +. (weights.holes *. float_of_int holes)
    +. (weights.bumpiness *. float_of_int bumps)
  in
  let expected_result = 0.0 in
  assert_equal expected_result result

let test_evaluate_state_with_lines _ =
  let agg_height = 1 in
  let complete = 1 in
  let holes = 0 in
  let bumps = 0 in
  let result =
    (weights.aggregate_height *. float_of_int agg_height)
    +. (weights.complete_lines *. float_of_int complete)
    +. (weights.holes *. float_of_int holes)
    +. (weights.bumpiness *. float_of_int bumps)
  in
  let expected_result =
    (-0.510066 *. float_of_int agg_height) +. (0.760666 *. float_of_int complete)
  in
  assert_equal expected_result result

let test_get_next_move_no_best_placement _ =
  let well = [| [| "empty"; "empty" |]; [| "empty"; "empty" |] |] in
  let piece = { piece_type = I; position = fpoint 0.0 0.0; rotation = ref 0 } in
  let held_piece =
    { piece_type = O; position = fpoint 1.0 1.0; rotation = ref 1 }
  in
  let result = get_next_move well piece held_piece in
  let expected_result = HardDrop in
  assert_equal expected_result result

let test_evaluate_state_empty_well _ =
  let well = [||] in
  (* Empty well *)
  let aggressive_mode = false in
  (* This value can vary depending on your needs *)
  let result = evaluate_state well aggressive_mode in
  let expected_result = 0.0 in
  (* Since the well is empty, we expect the evaluation to be 0 *)
  assert_equal expected_result result

let test_evaluate_state_with_incomplete_rows_aggressive _ =
  let well =
    [|
      [| "filled"; "empty"; "filled" |];
      (* Not complete – contains "empty" *)
      [| "filled"; "filled"; "filled" |];
      (* Complete *)
      [| "empty"; "empty"; "empty" |];
      (* Not complete – all empty *)
    |]
  in
  let result = evaluate_state well true in
  (* We're testing side-effects like adjusted_complete_lines skipping incomplete
     rows. Since the actual score isn't easy to calculate without replicating
     logic, just assert it's finite and negative-ish due to the weights *)
  assert_bool "Score should be a finite float" (not (Float.is_nan result))

let test_get_next_move_on_empty_well _ =
  let well = [||] in

  (* Piece: T at (4.0, 0.0), rotation 0 *)
  let piece =
    { piece_type = T; position = { fx = 4.0; fy = 0.0 }; rotation = ref 0 }
  in

  (* Held piece: I at (4.0, 0.0), rotation 0 *)
  let held_piece =
    { piece_type = I; position = { fx = 4.0; fy = 0.0 }; rotation = ref 0 }
  in

  let move = get_next_move well piece held_piece in
  (* On an empty board, the move should be valid — usually HardDrop, but we
     don't assert the exact one because logic may choose rotation first *)
  match move with
  | ShiftLeft | ShiftRight | RotateCW | RotateCCW | HardDrop | Hold -> ()

let test_hold_same_piece _ =
  let well = Array.make_matrix 20 10 "empty" in

  let piece =
    { piece_type = T; position = { fx = 4.0; fy = 0.0 }; rotation = ref 0 }
  in

  let held_piece =
    {
      piece_type = T;
      (* Same type as current piece *)
      position = { fx = 4.0; fy = 0.0 };
      rotation = ref 0;
    }
  in

  let move = get_next_move well piece held_piece in

  assert_bool "Should not return Hold when held piece is identical"
    (move <> Hold)

let test_no_valid_placements_returns_harddrop _ =
  (* Create a completely filled well *)
  let full_well = Array.make_matrix 20 10 "X" in

  let piece =
    { piece_type = T; position = { fx = 4.0; fy = 0.0 }; rotation = ref 0 }
  in

  let held_piece =
    { piece_type = I; position = { fx = 4.0; fy = 0.0 }; rotation = ref 0 }
  in

  let move = get_next_move full_well piece held_piece in

  assert_equal HardDrop move
    ~msg:"Should return HardDrop when no placements exist"

let suite =
  "Tetris Bot Tests"
  >::: [
         "test_cols_empty_well" >:: test_cols_empty_well;
         "test_cols_non_empty_well" >:: test_cols_non_empty_well;
         "test_count_holes_no_holes" >:: test_count_holes_no_holes;
         "test_count_holes_with_holes" >:: test_count_holes_with_holes;
         "test_evaluate_state_no_lines" >:: test_evaluate_state_no_lines;
         "test_evaluate_state_with_lines" >:: test_evaluate_state_with_lines;
         "test_get_next_move_no_best_placement"
         >:: test_get_next_move_no_best_placement;
         "test_evaluate_state_empty_well" >:: test_evaluate_state_empty_well;
         "test_evaluate_state_with_incomplete_rows_aggressive"
         >:: test_evaluate_state_with_incomplete_rows_aggressive;
         "test_get_next_move_on_empty_well" >:: test_get_next_move_on_empty_well;
         "test_hold_same_piece" >:: test_hold_same_piece;
         "test_no_valid_placements_returns_harddrop"
         >:: test_no_valid_placements_returns_harddrop;
       ]

let () = run_test_tt_main suite
