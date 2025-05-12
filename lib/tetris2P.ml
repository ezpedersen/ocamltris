type t = {
  left : Tetris.t;
  right : Tetris.t;
  mutable left_garbaged_lines : int;
  mutable right_garbaged_lines : int;
}

let create (cols, rows) =
  let left = Tetris.create (cols, rows) false 0 in
  let right = Tetris.create (cols, rows) true 4 in
  { left; right; left_garbaged_lines = 0; right_garbaged_lines = 0 }

let add_garbage g =
  let left_lines = Tetris.get_lines_cleared g.left in
  let right_lines = Tetris.get_lines_cleared g.right in
  if left_lines > g.left_garbaged_lines then begin
    let garbage = left_lines - g.left_garbaged_lines - 1 in
    if garbage > 0 then Tetris.add_garbage g.right garbage;
    g.left_garbaged_lines <- left_lines
  end;
  if right_lines > g.right_garbaged_lines then begin
    let garbage = right_lines - g.right_garbaged_lines - 1 in
    if garbage > 0 then Tetris.add_garbage g.left garbage;
    g.right_garbaged_lines <- right_lines
  end

let tick_left g =
  Tetris.tick g.left |> ignore;
  add_garbage g

let tick_right g =
  Tetris.tick g.right |> ignore;
  add_garbage g

let shift_left g n =
  Tetris.shift g.left n;
  add_garbage g

let shift_right g n =
  Tetris.shift g.right n;
  add_garbage g

let get_entry_left g (x, y) = Tetris.get_entry g.left (x, y)
let get_entry_right g (x, y) = Tetris.get_entry g.right (x, y)

let rotate_ccw_left g =
  Tetris.rotate_ccw g.left;
  add_garbage g

let rotate_ccw_right g =
  Tetris.rotate_ccw g.right;
  add_garbage g

let rotate_cw_left g =
  Tetris.rotate_cw g.left;
  add_garbage g

let rotate_cw_right g =
  Tetris.rotate_cw g.right;
  add_garbage g

let hard_drop_left g =
  Tetris.hard_drop g.left;
  add_garbage g

let hard_drop_right g =
  Tetris.hard_drop g.right;
  add_garbage g

let get_scores g = (Tetris.get_score g.left, Tetris.get_score g.right)
let get_held g = (Tetris.get_held g.left, Tetris.get_held g.right)

let hold_left g =
  Tetris.hold g.left;
  add_garbage g

let hold_right g =
  Tetris.hold g.right;
  add_garbage g

let is_game_over g = (Tetris.is_game_over g.left, Tetris.is_game_over g.right)

let apply_bot_move g =
  let _ = Tetris.apply_bot_move g.left in
  let _ = Tetris.apply_bot_move g.right in
  add_garbage g
