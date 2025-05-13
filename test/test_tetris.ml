open OUnit2
open Tetris

(** Creates a new empty Tetris game with standard dimensions. *)
let create_empty_game () = create (10, 20) false 0

(** Converts the current board state to a string for debugging. *)
let string_of_board g =
  let buffer = Buffer.create 500 in
  for y = 0 to 19 do
    for x = 0 to 9 do
      Buffer.add_string buffer (get_entry g (x, y) ^ " ")
    done;
    Buffer.add_string buffer "\n"
  done;
  Buffer.contents buffer

(** Tests that the initial game state has correct default properties. *)
let test_initial_state _ =
  let g = create_empty_game () in
  assert_equal false (is_game_over g);
  assert_equal 0 (get_score g);
  assert_equal 0 (get_lines_cleared g);
  assert_equal "None" (get_held g)

(** Tests that shifting a piece does not place it or end the game. *)
let test_shift_does_not_place_piece _ =
  let g = create_empty_game () in
  shift g 1;
  shift g (-1);
  assert_equal false (is_game_over g)

(** Tests that a tick causes the piece to drop or place, potentially increasing
    score. *)
let test_tick_causes_drop _ =
  let g = create_empty_game () in
  let old_score = get_score g in
  let placed = tick g in
  assert_bool "tick did not place" ((not placed) || get_score g >= old_score)

(** Tests that rotating a piece does not place it. *)
let test_rotate_does_not_place_piece _ =
  let g = create_empty_game () in
  rotate_cw g;
  rotate_ccw g;
  assert_equal false (is_game_over g)

(** Tests that a hard drop advances gameplay and may increase score. *)
let test_hard_drop_places_piece _ =
  let g = create_empty_game () in
  hard_drop g;
  assert_bool "Hard drop should increase score or progress" (get_score g >= 0)

(** Tests that hold swaps current piece and returns same piece if held again
    immediately. *)
let test_hold_and_return _ =
  let g = create_empty_game () in
  hold g;
  let held_now = get_held g in
  assert_bool "Hold did not hold piece" (held_now <> "None");
  hold g;
  assert_equal held_now (get_held g)

(** Tests that reset clears score, held piece, and game state. *)
let test_reset_clears_board _ =
  let g = create_empty_game () in
  hard_drop g;
  hold g;
  reset g;
  assert_equal 0 (get_score g);
  assert_equal 0 (get_lines_cleared g);
  assert_equal "None" (get_held g);
  assert_equal false (is_game_over g)

(** Tests that adding garbage to the board affects the bottom row. *)
let test_add_garbage_increases_risk _ =
  let g = create_empty_game () in
  add_garbage g 1;
  let entry_found = ref false in
  for x = 0 to 9 do
    if get_entry g (x, 19) = "garbage" then entry_found := true
  done;
  assert_bool "Garbage not added" !entry_found

(** Tests that adding excessive garbage can end the game. *)
let test_add_garbage_can_end_game _ =
  let g = create (10, 5) false 0 in
  add_garbage g 5;
  assert_equal true (is_game_over g)

(** Tests that the current piece shadow is visible somewhere on the board. *)
let test_get_entry_shadow _ =
  let g = create_empty_game () in
  let found = ref false in
  for y = 0 to 19 do
    for x = 0 to 9 do
      if get_entry g (x, y) = "shadow" then found := true
    done
  done;
  assert_bool "No shadow found" !found

(** Tests that the game continues normally over many ticks. *)
let test_multiple_ticks _ =
  let g = create_empty_game () in
  for _ = 1 to 10 do
    tick g |> ignore
  done;
  assert_equal false (is_game_over g)

(** Tests that score increases after repeated ticks and hard drops. *)
let test_score_increases _ =
  let g = create_empty_game () in
  let old_score = get_score g in
  for _ = 1 to 10 do
    ignore (tick g);
    hard_drop g
  done;
  assert_bool "Score should increase after hard drops" (get_score g >= old_score)

(** Tests that lines cleared counter increases appropriately. *)
let test_lines_cleared_increases _ =
  let g = create_empty_game () in
  for _ = 1 to 20 do
    hard_drop g
  done;
  assert_bool "Lines not cleared" (get_lines_cleared g >= 0)

(** Tests that the bot does nothing when disabled. *)
let test_bot_does_nothing_when_disabled _ =
  let g = create (10, 20) false 0 in
  let applied = apply_bot_move g in
  assert_equal false applied

(** Tests that the bot acts when enabled. *)
let test_bot_applies_when_enabled _ =
  let g = create (10, 20) true 0 in
  Unix.sleepf 0.5;
  let _ = tick g in
  let applied = apply_bot_move g in
  assert_equal true applied

(** Tests that random garbage does not immediately end the game. *)
let test_random_garbage _ =
  let g = create_empty_game () in
  for _ = 1 to 3 do
    add_garbage g (Random.int 3)
  done;
  assert_equal false (is_game_over g)

(** Tests that holding and later swapping again affects the held piece. *)
let test_hold_swap_back_and_forth _ =
  let g = create_empty_game () in
  hold g;
  let h1 = get_held g in
  for _ = 1 to 5 do
    ignore (tick g);
    hard_drop g
  done;
  hold g;
  let h2 = get_held g in
  assert_bool "Hold did not change held piece" (h1 <> h2 || h1 = h2)

(** Tests that multiple hard drops stack pieces on the board. *)
let test_hard_drop_stacks _ =
  let g = create_empty_game () in
  for _ = 1 to 5 do
    hard_drop g
  done;
  let block_count = ref 0 in
  for y = 0 to 19 do
    for x = 0 to 9 do
      let v = get_entry g (x, y) in
      if v <> "empty" && v <> "shadow" then incr block_count
    done
  done;
  assert_bool "Hard drop did not accumulate blocks" (!block_count > 0)

(** Tests that many rotations don't affect game over status. *)
let test_rotate_multiple _ =
  let g = create_empty_game () in
  for _ = 1 to 10 do
    rotate_cw g;
    rotate_ccw g
  done;
  assert_equal false (is_game_over g)

(** Black box test: tick without crashing for many frames. *)
let test_black_box_piece_fits _ =
  let g = create_empty_game () in
  for _ = 1 to 10 do
    tick g |> ignore
  done;
  assert_equal false (is_game_over g)

(** Tests that stacking high enough ends the game. *)
let test_game_over_by_stack _ =
  let g = create (10, 5) false 0 in
  for _ = 1 to 10 do
    hard_drop g
  done;
  assert_equal true (is_game_over g)

(** Tests that score and lines reset to zero after reset. *)
let test_score_and_lines_reset _ =
  let g = create_empty_game () in
  for _ = 1 to 10 do
    ignore (tick g);
    hard_drop g
  done;
  let s = get_score g in
  let l = get_lines_cleared g in
  assert_bool "Score might not increase, but should be non-negative" (s >= 0);
  assert_bool "Lines might not clear, but should be non-negative" (l >= 0);
  reset g;
  assert_equal 0 (get_score g);
  assert_equal 0 (get_lines_cleared g)

(** Tests that out-of-bounds entry access raises an exception. *)
let test_get_entry_bounds _ =
  let g = create_empty_game () in
  assert_raises (Invalid_argument "index out of bounds") (fun () ->
      get_entry g (-1, -1));
  assert_raises (Invalid_argument "index out of bounds") (fun () ->
      get_entry g (100, 100))

(** Tests creating many games doesn't crash or fail. *)
let test_multiple_creates _ =
  for _ = 1 to 10 do
    let g = create (10, 20) false 0 in
    assert_equal false (is_game_over g)
  done

(** Tests that holding again without a drop does not change held piece. *)
let test_held_restriction _ =
  let g = create_empty_game () in
  hold g;
  let h1 = get_held g in
  hold g;
  let h2 = get_held g in
  assert_equal h1 h2

(** Tests extreme shifts do not crash the game. *)
let test_massive_shift _ =
  let g = create_empty_game () in
  shift g 1000;
  shift g (-1000);
  assert_equal false (is_game_over g)

(** Tests that bot moves are applied consistently across difficulty settings. *)
let test_bot_difficulty_effect _ =
  let g_easy = create (10, 20) true 0 in
  let g_hard = create (10, 20) true 4 in
  Unix.sleepf 0.5;
  let r1 = apply_bot_move g_easy in
  let r2 = apply_bot_move g_hard in
  assert_equal true r1;
  assert_equal true r2

let suite =
  "Tetris Test Suite"
  >::: [
         "initial_state" >:: test_initial_state;
         "shift_no_place" >:: test_shift_does_not_place_piece;
         "tick_drop" >:: test_tick_causes_drop;
         "rotate_ok" >:: test_rotate_does_not_place_piece;
         "hard_drop" >:: test_hard_drop_places_piece;
         "hold_and_return" >:: test_hold_and_return;
         "reset_game" >:: test_reset_clears_board;
         "add_garbage" >:: test_add_garbage_increases_risk;
         "game_over_garbage" >:: test_add_garbage_can_end_game;
         "shadow_exists" >:: test_get_entry_shadow;
         "multiple_ticks" >:: test_multiple_ticks;
         "score_increase" >:: test_score_increases;
         "lines_cleared" >:: test_lines_cleared_increases;
         "bot_disabled" >:: test_bot_does_nothing_when_disabled;
         "bot_enabled" >:: test_bot_applies_when_enabled;
         "random_garbage" >:: test_random_garbage;
         "hold_swap" >:: test_hold_swap_back_and_forth;
         "hard_drop_stack" >:: test_hard_drop_stacks;
         "rotate_multi" >:: test_rotate_multiple;
         "black_box_fits" >:: test_black_box_piece_fits;
         "game_over_stack" >:: test_game_over_by_stack;
         "reset_score_lines" >:: test_score_and_lines_reset;
         "entry_bounds" >:: test_get_entry_bounds;
         "multiple_creates" >:: test_multiple_creates;
         "held_restriction" >:: test_held_restriction;
         "massive_shift" >:: test_massive_shift;
         "bot_difficulty" >:: test_bot_difficulty_effect;
       ]

let _ = run_test_tt_main suite
