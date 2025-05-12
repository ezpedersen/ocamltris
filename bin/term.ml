open Lwt.Infix

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
  for x = 0 to 50 do
    print_newline ()
  done;
  Printf.fprintf stdout "Score: %d | Held: %s \n" (Tetris.get_score game)
    (Tetris.get_held game);
  for i = 0 to 19 do
    for j = 0 to 9 do
      print_string
        (match Tetris.get_entry game (j, i) with
        | "empty" -> "."
        | "shadow" -> "!"
        | _ -> "#")
    done;
    print_newline ()
  done

let game_over_msg () =
  print_endline "Game Over! Press 'q' to quit or 'r' to restart."

let rec render_loop game =
  if Tetris.is_game_over game then (
    for x = 0 to 50 do
      print_newline ()
    done;
    game_over_msg ();
    Lwt.return_unit)
  else
    Lwt_unix.sleep 1.0 >>= fun () ->
    ignore (Tetris.tick game);
    render game;
    render_loop game

let rec input_loop game =
  Lwt_io.read_char Lwt_io.stdin >>= fun c ->
  let is_over = Tetris.is_game_over game in
  match c with
  | 'q' -> Lwt.fail Exit
  | 'r' when is_over ->
      Tetris.reset game;
      print_endline "Game restarted!";
      render game;
      Lwt.join [ render_loop game; input_loop game ]
  | _ when is_over -> input_loop game
  | 'h' ->
      Tetris.shift game (-1);
      render game;
      input_loop game
  | 'l' ->
      Tetris.shift game 1;
      render game;
      input_loop game
  | 'a' ->
      Tetris.rotate_ccw game;
      render game;
      input_loop game
  | 'd' | 'k' ->
      Tetris.rotate_cw game;
      render game;
      input_loop game
  | 's' ->
      Tetris.hold game;
      render game;
      input_loop game
  | 'j' ->
      ignore (Tetris.tick game);
      render game;
      input_loop game
  | ' ' ->
      Tetris.hard_drop game;
      render game;
      input_loop game
  | _ -> input_loop game

let () =
  let original_termios = Unix.tcgetattr Unix.stdin in
  enable_raw_mode ();
  print_endline "Raw mode enabled. Press 'q' to quit.";
  let game = Tetris.create (10, 20) false 0 in
  Lwt_main.run
    (Lwt.catch
       (fun () -> Lwt.join [ render_loop game; input_loop game ])
       (fun exn ->
         disable_raw_mode original_termios;
         print_endline (Printexc.to_string exn);
         Lwt.return_unit))
(* with Exit -> *)
(*   disable_raw_mode original_termios; *)
(*   print_endline "Raw mode disabled." *)
