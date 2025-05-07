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

let rec input_loop game =
  let after =
   fun () ->
    render game;
    input_loop game
  in
  Lwt_io.read_char Lwt_io.stdin >>= function
  | 'q' -> Lwt.fail Exit
  | 'h' ->
      Tetris.shift_right game (-1);
      after ()
  | 'l' ->
      Tetris.shift_right game 1;
      after ()
  | 'a' ->
      Tetris.rotate_ccw game;
      after ()
  | 'd' ->
      Tetris.rotate_cw game;
      after ()
  | 's' ->
      Tetris.hold game;
      after ()
  | 'k' ->
      Tetris.rotate_cw game;
      after ()
  | 'j' ->
      Tetris.tick game |> ignore;
      after ()
  | ' ' ->
      Tetris.hard_drop game;
      after ()
  | _ -> after ()

let rec render_loop game =
  Lwt_unix.sleep 1.0 >>= fun () ->
  Tetris.tick game |> ignore;
  render game;
  render_loop game

let () =
  let original_termios = Unix.tcgetattr Unix.stdin in
  enable_raw_mode ();
  print_endline "Raw mode enabled. Press 'q' to quit.";
  let game = Tetris.create (10, 20) in
  Lwt_main.run
    (Lwt.catch
       (fun () -> Lwt.pick [ render_loop game; input_loop game ])
       (function
         | exn ->
         disable_raw_mode original_termios;
         print_endline (Printexc.to_string exn);
         Lwt.return_unit))
(* with Exit -> *)
(*   disable_raw_mode original_termios; *)
(*   print_endline "Raw mode disabled." *)
