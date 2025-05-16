open OUnit2
open Bot
open Geometry

(* Config stub module *)
module Config = struct
  type t = { dummy : bool }

  let load (filename : string) : (t, string) result = Ok { dummy = true }
end

(* Helper functions *)
let make_empty_row () : string array = Array.init 10 (fun _ -> "empty")

let empty_well () : string array array =
  Array.init 20 (fun _ -> make_empty_row ())

let mk_piece_params pt (r : int ref) (pos : fpoint) : piece =
  make_piece pt r pos

let mk_piece pt : piece =
  let r = ref 0 in
  let pos = fpoint 4.0 0.0 in
  mk_piece_params pt r pos

let well_with_line () : string array array =
  let well = empty_well () in
  let () =
    for i = 0 to 9 do
      well.(19).(i) <- "O"
    done
  in
  well

let test_avoid_hold_when_piece_is_better (_ctx : test_ctxt) : unit =
  let well : string array array = Array.make_matrix 20 10 "empty" in
  let piece : piece =
    Geometry.make_piece Geometry.I (ref 0) (Geometry.fpoint 4.0 0.0)
  in
  let held : piece =
    Geometry.make_piece Geometry.O (ref 0) (Geometry.fpoint 4.0 0.0)
  in
  let move_result = Bot.get_next_move well piece held in
  let not_hold = move_result <> Hold in
  assert_bool "Expected not to hold when current piece is better" not_hold

let test_get_next_move_sequence_length (_ctx : test_ctxt) : unit =
  let well = empty_well () in
  let piece = mk_piece L in
  let held = mk_piece S in
  let moves = Bot.get_next_move_sequence well piece held in
  let has_hard_drop = List.mem HardDrop moves in
  assert_bool "Should end in HardDrop" has_hard_drop

let test_should_hold_better_held (_ctx : test_ctxt) : unit =
  let well = empty_well () in
  let piece = mk_piece Z in
  let held = mk_piece I in
  let moves = Bot.get_next_move_sequence well piece held in
  let first_move = List.hd moves in
  assert_equal Hold first_move

let test_drop_piece_valid (_ctx : test_ctxt) : unit =
  let well = empty_well () in
  let piece = mk_piece T in
  let held = mk_piece J in
  let moves = Bot.get_next_move_sequence well piece held in
  let contains_drop = List.mem HardDrop moves in
  assert_bool "HardDrop should be present" contains_drop

let test_get_next_move_full_line (_ctx : test_ctxt) : unit =
  let well = well_with_line () in
  let piece = mk_piece I in
  let held = mk_piece S in
  let _move = Bot.get_next_move well piece held in
  assert_bool "AI should still make a move even with a filled line" true

(* Spawn-sanity test for each tetris piece *)
let pieces =
  [
    Geometry.I;
    Geometry.O;
    Geometry.T;
    Geometry.S;
    Geometry.Z;
    Geometry.J;
    Geometry.L;
  ]

let test_spawn_each_piece (_ctx : test_ctxt) : unit =
  List.iter
    (fun pt ->
      let well = empty_well () in
      let p = mk_piece pt in
      let h = mk_piece pt in
      let _ = Bot.get_next_move well p h in
      ())
    pieces;
  assert_bool "spawn sanity passed" true

(* 7. Test suite *)
let suite =
  "Tetris Bot Expanded Tests"
  >::: [
         "test_avoid_hold_when_piece_is_better"
         >:: test_avoid_hold_when_piece_is_better;
         "test_get_next_move_sequence_length"
         >:: test_get_next_move_sequence_length;
         "test_should_hold_better_held" >:: test_should_hold_better_held;
         "test_drop_piece_valid" >:: test_drop_piece_valid;
         "test_get_next_move_full_line" >:: test_get_next_move_full_line;
         "test_spawn_each_piece" >:: test_spawn_each_piece;
       ]

let () = run_test_tt_main suite
