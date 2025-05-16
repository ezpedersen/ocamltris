[@@@coverage exclude_file]
(* GUI is moved to lib solely for the purpose of using it in main. It contains
   mainly frontend code so doesn't make sense to test coverage with bisect. *)

open OcamlCanvas.V1

type gui_state =
  | Title
  | Game of Tetris.t
  | GameOver of Tetris.t
  | Pause of Tetris.t
  | Game2P of Tetris2P.t
  | GameOver2P of Tetris2P.t
  | Pause2P of Tetris2P.t
  | BotSettings
  | Controls

let current_state = ref Title
let cell_size = 30.
let cols = 10
let rows = 20
let tick_interval = 1.0
let board_width = float_of_int cols *. cell_size
let board_height = float_of_int rows *. cell_size
let window_width = (board_width *. 2.0) +. 400.0
let window_height = board_height +. 200.0
let left_board_x = 100.0
let right_board_x = board_width +. 200.0
let board_y = 100.0
let center_x = window_width /. 2.0

let render_title_screen c =
  Canvas.setFillColor c Color.black;
  Canvas.fillRect c ~pos:(0.0, 0.0) ~size:(window_width, window_height);
  Canvas.setFillColor c Color.white;
  Canvas.fillText c "2-Player Ocamltris" (center_x -. 150.0, 100.0);
  Canvas.fillText c "Press 0 to play singleplayer." (center_x -. 400.0, 150.0);
  Canvas.fillText c
    "Press 1-4 to play against a bot (difficulty 1-4 respectively)."
    (center_x -. 400.0, 200.0);
  Canvas.fillText c "Press 5 to play local multiplayer."
    (center_x -. 400.0, 250.0);
  Canvas.fillText c "Press 6 to watch the max difficulty bot play by itself!"
    (center_x -. 400.0, 300.0);
  Canvas.fillText c "Press 7 to watch two max difficulty bots play each other."
    (center_x -. 400.0, 350.0);
  Canvas.fillText c "Press F1 for controls" (center_x -. 400.0, 400.0);
  Canvas.show c

let render_bot_settings c =
  Canvas.setFillColor c Color.black;
  Canvas.fillRect c ~pos:(0.0, 0.0) ~size:(window_width, window_height);
  Canvas.setFillColor c Color.white;
  Canvas.fillText c "2-Player Settings" (center_x -. 150.0, 100.0);
  Canvas.fillText c
    "Press 1-4 to play against a bot (difficulty 1-4 respectively)."
    (center_x -. 400.0, 200.0);
  Canvas.fillText c "Press 6 to watch the max difficulty bot play by itself!"
    (center_x -. 400.0, 300.0);
  Canvas.fillText c "Press 7 to watch two max difficulty bots play each other."
    (center_x -. 400.0, 350.0);
  Canvas.show c

let render_controls c =
  Canvas.setFillColor c Color.black;
  Canvas.fillRect c ~pos:(0., 0.)
    ~size:
      ( (float_of_int cols *. cell_size *. 2.) +. 400.,
        (float_of_int rows *. cell_size) +. 200. );
  Canvas.setFillColor c Color.white;
  Canvas.fillText c "CONTROLS" ((float_of_int cols *. cell_size) +. 100., 80.);

  Canvas.fillText c "Player 1:" (150., 140.);
  Canvas.fillText c "A/D: Move Left/Right" (150., 180.);
  Canvas.fillText c "W/S: Rotate CW/CCW" (150., 220.);
  Canvas.fillText c "Z: Soft Drop" (150., 260.);
  Canvas.fillText c "X: Hard Drop" (150., 300.);
  Canvas.fillText c "C: Hold" (150., 340.);

  Canvas.fillText c "Player 2:" ((float_of_int cols *. cell_size) +. 350., 140.);
  Canvas.fillText c "J/L: Move Left/Right"
    ((float_of_int cols *. cell_size) +. 350., 180.);
  Canvas.fillText c "I/K: Rotate CW/CCW"
    ((float_of_int cols *. cell_size) +. 350., 220.);
  Canvas.fillText c "M: Soft Drop"
    ((float_of_int cols *. cell_size) +. 350., 260.);
  Canvas.fillText c ". (period): Hold"
    ((float_of_int cols *. cell_size) +. 350., 300.);
  Canvas.fillText c ", (comma): Hard Drop"
    ((float_of_int cols *. cell_size) +. 350., 340.);

  Canvas.fillText c "P: Pause" ((float_of_int cols *. cell_size) +. 100., 400.);
  Canvas.fillText c "Q: Quit" ((float_of_int cols *. cell_size) +. 100., 440.);
  Canvas.fillText c "Press any key to return"
    ((float_of_int cols *. cell_size) +. 100., 500.)

let render_game c game =
  Canvas.setFillColor c Color.black;
  Canvas.fillRect c ~pos:(0., 0.) ~size:(window_width, window_height);

  for i = 0 to rows - 1 do
    for j = 0 to cols - 1 do
      let entry = Tetris.get_entry game (j, i) in
      let color =
        match entry with
        | "empty" -> Color.black
        | "I" -> Color.cyan
        | "J" -> Color.blue
        | "Z" -> Color.red
        | "O" -> Color.of_string "#ffff00"
        | "S" -> Color.of_string "#00ff00"
        | "T" -> Color.of_string "#cc00ff"
        | "L" -> Color.orange
        | "garbage" -> Color.of_string "#69696d"
        | "shadow" -> Color.white
        | _ -> Color.black
      in
      let x = float_of_int j *. cell_size in
      let y = float_of_int i *. cell_size in
      Canvas.setFillColor c color;

      Canvas.fillRect c ~pos:(x, y) ~size:(cell_size, cell_size);
      Canvas.setStrokeColor c (Color.of_string "#3d3846");
      Canvas.strokeRect c ~pos:(x, y) ~size:(cell_size, cell_size)
    done
  done;
  Canvas.setFillColor c Color.cyan;
  Canvas.fillText c
    (Printf.sprintf "Score: %d" (Tetris.get_score game))
    ((float_of_int cols *. cell_size) +. 20., 10. +. 30.);

  Canvas.fillText c
    (Printf.sprintf "Held: %s" (Tetris.get_held game))
    ((float_of_int cols *. cell_size) +. 20., 60. +. 30.)

let render_multiplayer_game c game =
  Canvas.setFillColor c Color.black;
  Canvas.fillRect c ~pos:(0., 0.)
    ~size:
      ( (float_of_int cols *. cell_size *. 2.) +. 400.,
        (float_of_int rows *. cell_size) +. 200. );

  let left_board_x = 100. in
  let board_y = 100. in
  let right_board_x = (float_of_int cols *. cell_size) +. 200. in

  Canvas.setFillColor c Color.white;
  Canvas.fillText c "Player 1" (left_board_x +. 40., 50.);
  Canvas.fillText c "Player 2" (right_board_x, 50.);

  let left_score, right_score = Tetris2P.get_scores game in
  Canvas.setFillColor c Color.cyan;
  Canvas.fillText c
    (Printf.sprintf "Score: %d" left_score)
    (left_board_x -. 90., board_y +. 100.);
  Canvas.fillText c
    (Printf.sprintf "Score: %d" right_score)
    (right_board_x +. (float_of_int cols *. cell_size) +. 35., board_y +. 100.);

  let left_held, right_held = Tetris2P.get_held game in
  Canvas.fillText c
    (Printf.sprintf "Held: %s" left_held)
    (left_board_x -. 90., board_y +. 150.);
  Canvas.fillText c
    (Printf.sprintf "Held: %s" right_held)
    (right_board_x +. (float_of_int cols *. cell_size) +. 35., board_y +. 150.);

  for i = 0 to rows - 1 do
    for j = 0 to cols - 1 do
      let entry = Tetris2P.get_entry_left game (j, i) in
      let color =
        match entry with
        | "empty" -> Color.black
        | "I" -> Color.cyan
        | "J" -> Color.blue
        | "Z" -> Color.red
        | "O" -> Color.of_string "#ffff00"
        | "S" -> Color.of_string "#00ff00"
        | "T" -> Color.of_string "#cc00ff"
        | "L" -> Color.orange
        | "garbage" -> Color.of_string "#69696d"
        | "shadow" -> Color.white
        | _ -> Color.black
      in
      let x = left_board_x +. (float_of_int j *. cell_size) +. 70. in
      let y = board_y +. (float_of_int i *. cell_size) in
      Canvas.setFillColor c color;

      Canvas.fillRect c ~pos:(x, y) ~size:(cell_size, cell_size);
      Canvas.setStrokeColor c (Color.of_string "#3d3846");
      Canvas.strokeRect c ~pos:(x, y) ~size:(cell_size, cell_size)
    done
  done;

  for i = 0 to rows - 1 do
    for j = 0 to cols - 1 do
      let entry = Tetris2P.get_entry_right game (j, i) in
      let color =
        match entry with
        | "empty" -> Color.black
        | "I" -> Color.cyan
        | "J" -> Color.blue
        | "Z" -> Color.red
        | "O" -> Color.of_string "#ffff00"
        | "S" -> Color.of_string "#00ff00"
        | "T" -> Color.of_string "#cc00ff"
        | "L" -> Color.orange
        | "garbage" -> Color.of_string "#69696d"
        | "shadow" -> Color.white
        | _ -> Color.black
      in
      let x = right_board_x +. (float_of_int j *. cell_size) +. 15. in
      let y = board_y +. (float_of_int i *. cell_size) in
      Canvas.setFillColor c color;

      Canvas.fillRect c ~pos:(x, y) ~size:(cell_size, cell_size);
      Canvas.setStrokeColor c (Color.of_string "#3d3846");
      Canvas.strokeRect c ~pos:(x, y) ~size:(cell_size, cell_size)
    done
  done

let render c =
  match !current_state with
  | Game g -> render_game c g
  | Title -> render_title_screen c
  | BotSettings -> render_bot_settings c
  | GameOver g ->
      render_game c g;
      Canvas.setFillColor c Color.red;
      Canvas.fillText c "Game Over!" (float_of_int cols *. cell_size /. 2., 100.);
      Canvas.fillText c "Press R to go back to the title screen"
        (float_of_int cols *. cell_size /. 2., 150.)
  | Pause g ->
      render_game c g;
      Canvas.setFillColor c Color.green;
      Canvas.fillText c "Paused" (float_of_int cols *. cell_size /. 2., 100.);
      Canvas.fillText c "Press P to resume"
        (float_of_int cols *. cell_size /. 2., 150.)
  | Controls -> render_controls c
  | Game2P g -> render_multiplayer_game c g
  | GameOver2P g ->
      render_multiplayer_game c g;
      let left_game_over, right_game_over = Tetris2P.is_game_over g in
      Canvas.setFillColor c Color.red;
      if left_game_over && right_game_over then
        Canvas.fillText c "Tie for both players."
          ((float_of_int cols *. cell_size) +. 100., 50.)
      else if left_game_over then
        Canvas.fillText c "Player 2 >>> Player 1!"
          ((float_of_int cols *. cell_size) +. 100., 50.)
      else if right_game_over then
        Canvas.fillText c "Player 1 >>> Player 2!"
          ((float_of_int cols *. cell_size) +. 100., 50.);
      Canvas.fillText c "Press R to go back to the title screen"
        ((float_of_int cols *. cell_size) +. 100., 90.)
  | Pause2P g ->
      render_multiplayer_game c g;
      Canvas.setFillColor c Color.green;
      Canvas.fillText c "Paused" ((float_of_int cols *. cell_size) +. 100., 200.);
      Canvas.fillText c "Press P to resume"
        ((float_of_int cols *. cell_size) +. 100., 250.)

let run_gui () =
  Backend.init ();

  let width = int_of_float (float_of_int cols *. cell_size) in
  let height = int_of_float (float_of_int rows *. cell_size) in
  let c =
    Canvas.createOnscreen ~title:"Ocamltris" ~pos:(300, 200)
      ~size:((2 * width) + 400, height + 200)
      ()
  in
  Canvas.setFont c "Liberation Sans" ~size:30.0 ~slant:Font.Roman
    ~weight:Font.bold;
  Canvas.setLineWidth c 2.;

  let last_tick_p1 = ref (Unix.gettimeofday ()) in
  let last_tick_p2 = ref (Unix.gettimeofday ()) in
  render_title_screen c;
  Canvas.show c;
  let stop_on_close = React.E.map (fun _ -> Backend.stop ()) Event.close in

  let controls =
    React.E.map
      (fun { Event.data = { Event.key; _ }; _ } ->
        match !current_state with
        | Game g ->
            let _ =
              match key with
              | Event.KeyQ ->
                  Backend.stop ();
                  Canvas.close c
              | Event.KeyA -> Tetris.shift g (-1)
              | Event.KeyD -> Tetris.shift g 1
              | Event.KeyW -> Tetris.rotate_cw g
              | Event.KeyS -> Tetris.rotate_ccw g
              | Event.KeyC -> Tetris.hold g
              | Event.KeyZ ->
                  ignore (Tetris.tick g);
                  last_tick_p1 := Unix.gettimeofday ()
              | Event.KeyX -> Tetris.hard_drop g
              | Event.KeyP -> current_state := Pause g
              | _ -> ()
            in
            render c
        | Title -> (
            match key with
            | Event.Key0RParenthesis ->
                let game = Tetris.create (cols, rows) false 0 in
                current_state := Game game;
                last_tick_p1 := Unix.gettimeofday ();
                render c
            | Event.KeyF1 ->
                current_state := Controls;
                render c
            | Event.Key1Exclamation ->
                let game = Tetris2P.create (cols, rows) false 0 true 1 in
                current_state := Game2P game;
                last_tick_p1 := Unix.gettimeofday ();
                last_tick_p2 := Unix.gettimeofday ();
                render c
            | Event.Key2At ->
                let game = Tetris2P.create (cols, rows) false 0 true 2 in
                current_state := Game2P game;
                last_tick_p1 := Unix.gettimeofday ();
                last_tick_p2 := Unix.gettimeofday ();
                render c
            | Event.Key3Number ->
                let game = Tetris2P.create (cols, rows) false 0 true 3 in
                current_state := Game2P game;
                last_tick_p1 := Unix.gettimeofday ();
                last_tick_p2 := Unix.gettimeofday ();
                render c
            | Event.Key4Dollar ->
                let game = Tetris2P.create (cols, rows) false 0 true 4 in
                current_state := Game2P game;
                last_tick_p1 := Unix.gettimeofday ();
                last_tick_p2 := Unix.gettimeofday ();
                render c
            | Event.Key5Percent ->
                let game = Tetris2P.create (cols, rows) false 0 false 0 in
                current_state := Game2P game;
                last_tick_p1 := Unix.gettimeofday ();
                last_tick_p2 := Unix.gettimeofday ();
                render c
            | Event.Key6Caret ->
                let game = Tetris.create (cols, rows) true 4 in
                current_state := Game game;
                last_tick_p1 := Unix.gettimeofday ();
                render c
            | Event.Key7Ampersand ->
                let game = Tetris2P.create (cols, rows) true 4 true 4 in
                current_state := Game2P game;
                last_tick_p1 := Unix.gettimeofday ();
                last_tick_p2 := Unix.gettimeofday ();
                render c
            | _ -> ())
        | Pause g -> (
            match key with
            | Event.KeyP ->
                current_state := Game g;
                last_tick_p1 := Unix.gettimeofday ();
                render c
            | _ -> ())
        | GameOver g -> (
            match key with
            | Event.KeyR ->
                current_state := Title;
                render c
            | _ -> ())
        | Game2P g ->
            let _ =
              match key with
              | Event.KeyQ ->
                  Backend.stop ();
                  Canvas.close c
              | Event.KeyA -> Tetris2P.shift_left g (-1)
              | Event.KeyD -> Tetris2P.shift_left g 1
              | Event.KeyW -> Tetris2P.rotate_cw_left g
              | Event.KeyS -> Tetris2P.rotate_ccw_left g
              | Event.KeyC -> Tetris2P.hold_left g
              | Event.KeyZ ->
                  ignore (Tetris2P.tick_left g);
                  last_tick_p1 := Unix.gettimeofday ()
              | Event.KeyX -> Tetris2P.hard_drop_left g
              | Event.KeyP -> current_state := Pause2P g
              | Event.KeyJ -> Tetris2P.shift_right g (-1)
              | Event.KeyL -> Tetris2P.shift_right g 1
              | Event.KeyI -> Tetris2P.rotate_cw_right g
              | Event.KeyK -> Tetris2P.rotate_ccw_right g
              | Event.KeyPeriodGreater -> Tetris2P.hold_right g
              | Event.KeyM ->
                  ignore (Tetris2P.tick_right g);
                  last_tick_p2 := Unix.gettimeofday ()
              | Event.KeyCommaLess -> Tetris2P.hard_drop_right g
              | _ -> ()
            in
            render c
        | Pause2P g -> (
            match key with
            | Event.KeyP ->
                current_state := Game2P g;
                last_tick_p1 := Unix.gettimeofday ();
                last_tick_p2 := Unix.gettimeofday ();
                render c
            | _ -> ())
        | GameOver2P g -> (
            match key with
            | Event.KeyR ->
                current_state := Title;
                render c
            | _ -> ())
        | BotSettings ->
            current_state := BotSettings;
            render c
        | Controls ->
            current_state := Title;
            render c)
      Event.key_down
  in
  let _ =
    React.E.map
      (fun { Event.timestamp; _ } ->
        match !current_state with
        | Game g ->
            (if Tetris.is_game_over g then current_state := GameOver g
             else
               let now = Unix.gettimeofday () in
               let delta_time = now -. !last_tick_p1 in
               if delta_time >= tick_interval then begin
                 ignore (Tetris.tick g);
                 last_tick_p1 := now
               end;
               ignore (Tetris.apply_bot_move g));
            render c
        | Game2P g ->
            (let left_game_over, right_game_over = Tetris2P.is_game_over g in
             if left_game_over || right_game_over then
               current_state := GameOver2P g
             else
               let now = Unix.gettimeofday () in
               let delta_time_p1 = now -. !last_tick_p1 in
               let delta_time_p2 = now -. !last_tick_p2 in
               if delta_time_p1 >= tick_interval then begin
                 ignore (Tetris2P.tick_left g);
                 last_tick_p1 := now
               end;
               if delta_time_p2 >= tick_interval then begin
                 ignore (Tetris2P.tick_right g);
                 last_tick_p2 := now
               end;
               ignore (Tetris2P.apply_bot_move g));
            render c
        | _ -> ())
      Event.frame
  in
  Backend.run (fun () ->
      ignore stop_on_close;
      ignore controls)

let singleplayer () =
  let game = Tetris.create (cols, rows) false 0 in
  current_state := Game game;
  run_gui ()

let multiplayer () =
  let game = Tetris2P.create (cols, rows) false 0 false 0 in
  current_state := Game2P game;
  run_gui ()

let botplayer () =
  let game = Tetris2P.create (cols, rows) false 0 false 0 in
  current_state := Game2P game;
  run_gui ()
