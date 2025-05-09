type t = {
  left : Tetris.t;
  right : Tetris.t;
  mutable left_garbaged_score : int;
  mutable right_garbaged_score : int;
}

let create (cols, rows) =
  let left = Tetris.create (cols, rows) in
  let right = Tetris.create (cols, rows) in
  { left; right; left_garbaged_score = 0; right_garbaged_score = 0 }

let add_garbage g =
  let left_garbage = g.left_garbaged_score in
  let right_garbage = g.right_garbaged_score in
  let left_score = Tetris.get_score g.left in
  let right_score = Tetris.get_score g.right in
  if left_score > right_score then begin
    let garbage = left_score - left_garbage - 1 in
    Tetris.add_garbage g.right garbage;
    g.left_garbaged_score <- left_score
  end;
  if right_score > left_score then begin
    let garbage = right_score - right_garbage - 1 in
    Tetris.add_garbage g.left garbage;
    g.right_garbaged_score <- right_score
  end

let tick_left g = Tetris.tick g.left |> ignore
let tick_right g = Tetris.tick g.right |> ignore
let shift_left g n = Tetris.shift g.left n
let shift_right g n = Tetris.shift g.right n
let get_entry_left g (x, y) = Tetris.get_entry g.left (x, y)
let get_entry_right g (x, y) = Tetris.get_entry g.right (x, y)
let rotate_ccw_left g = Tetris.rotate_ccw g.left
let rotate_ccw_right g = Tetris.rotate_ccw g.right
let rotate_cw_left g = Tetris.rotate_cw g.left
let rotate_cw_right g = Tetris.rotate_cw g.right
let hard_drop_left g = Tetris.hard_drop g.left
let hard_drop_right g = Tetris.hard_drop g.right
let get_scores g = (Tetris.get_score g.left, Tetris.get_score g.right)
let get_held g = (Tetris.get_held g.left, Tetris.get_held g.right)
let hold_left g = Tetris.hold g.left
let hold_right g = Tetris.hold g.right
let is_game_over g = (Tetris.is_game_over g.left, Tetris.is_game_over g.right)
