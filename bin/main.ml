(* changes terminal to raw mode, make sure to disable after use *)
let enable_raw_mode () =
  let open Unix in
  let termios = tcgetattr stdin in
  let raw = { termios with c_icanon = false; c_echo = false } in
  tcsetattr stdin TCSANOW raw

(* changes terminal from raw to cooked mode *)
let disable_raw_mode termios =
  let open Unix in
  tcsetattr stdin TCSANOW termios

let render game =
  for i = 0 to 50 do
    print_newline ()
  done;
  for i = 0 to 19 do
    for j = 0 to 9 do
      print_string (if Tetris.get_entry game (j, i) then "#" else ".")
    done;
    print_newline ()
  done

let handle_input game =
  let char = input_char stdin in
  if char = 'q' then raise Exit;
  if char = 'h' then Tetris.shift_right game (-1);
  if char = 'l' then Tetris.shift_right game 1;
  if char = 'a' then Tetris.rotate_ccw game;
  if char = 'd' then Tetris.rotate_cw game;
  if char = 'j' then Tetris.tick game

let game_loop game =
  Tetris.tick game;
  handle_input game;
  render game;
  ()

let () =
  let original_termios = Unix.tcgetattr Unix.stdin in
  enable_raw_mode ();
  print_endline "Raw mode enabled. Press 'q' to quit.";
  let game = Tetris.create (10, 20) in
  try
    while true do
      game_loop game
    done
  with Exit ->
    disable_raw_mode original_termios;
    print_endline "Raw mode disabled."
