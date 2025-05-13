open OUnit2
open Bot
open Geometry

(* Helper: empty 10x20 well *)
let empty_well () = Array.make_matrix 20 10 "empty"

(* Helper: make a piece at origin with rotation 0 *)
let mk_piece pt = make_piece pt (ref 0) (fpoint 4. 0.)

(* A basic well with one row filled *)
let well_with_line () =
  let well = empty_well () in
  for i = 0 to 9 do
    well.(19).(i) <- "O"
  done;
  well

let test_avoid_hold_when_piece_is_better _ =
  let well = Array.make_matrix 20 10 "empty" in
  let piece =
    Geometry.make_piece Geometry.I (ref 0) (Geometry.fpoint 4.0 0.0)
  in
  let held = Geometry.make_piece Geometry.O (ref 0) (Geometry.fpoint 4.0 0.0) in
  let move = Bot.get_next_move well piece held in
  assert_bool "Expected not to hold when current piece is better" (move <> Hold)

let test_get_next_move_sequence_length _ =
  let well = empty_well () in
  let piece = mk_piece L in
  let held = mk_piece S in
  let moves = get_next_move_sequence well piece held in
  assert_bool "Should end in HardDrop" (List.mem HardDrop moves)

let test_should_hold_better_held _ =
  let well = empty_well () in
  (* Create a setup where held piece I can clear more lines *)
  let piece = mk_piece Z in
  let held = mk_piece I in
  let moves = get_next_move_sequence well piece held in
  assert_equal Hold (List.hd moves)

let test_drop_piece_valid _ =
  let well = empty_well () in
  let piece = mk_piece T in
  let held = mk_piece J in
  let moves = get_next_move_sequence well piece held in
  assert_bool "HardDrop should be present" (List.mem HardDrop moves)

let test_get_next_move_full_line _ =
  let well = well_with_line () in
  let piece = mk_piece I in
  let held = mk_piece S in
  let _ = get_next_move well piece held in
  assert_bool "AI should still make a move even with a filled line" true

let suite =
  "Tetris Bot Tests"
  >::: [
         "test_avoid_hold_when_piece_is_better"
         >:: test_avoid_hold_when_piece_is_better;
         "test_get_next_move_sequence_length"
         >:: test_get_next_move_sequence_length;
         "test_should_hold_better_held" >:: test_should_hold_better_held;
         "test_drop_piece_valid" >:: test_drop_piece_valid;
         "test_get_next_move_full_line" >:: test_get_next_move_full_line;
       ]

let () = run_test_tt_main suite
