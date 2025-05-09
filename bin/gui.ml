open OcamlCanvas.V1

type gui_state =
  | Title
  | Game of Tetris.t
  | GameOver of Tetris.t
  | Pause of Tetris.t
  | About

let current_state = ref Title
let cell_size = 30.
let cols = 10
let rows = 20
let tick_interval = 1.0

let render_title_screen c =
  Canvas.setFillColor c Color.black;
  Canvas.fillRect c ~pos:(0., 0.)
    ~size:
      ((float_of_int cols *. cell_size) +. 400., float_of_int rows *. cell_size);
  Canvas.setFillColor c Color.white;
  Canvas.fillText c "Welcome to Ocamltris!"
    ((float_of_int cols *. cell_size /. 2.) -. 100., 100.);
  Canvas.fillText c "Press SPACE to start"
    ((float_of_int cols *. cell_size /. 2.) -. 100., 150.);
  Canvas.show c

let render_game c game =
  Canvas.setFillColor c Color.black;
  Canvas.fillRect c ~pos:(0., 0.)
    ~size:
      ((float_of_int cols *. cell_size) +. 400., float_of_int rows *. cell_size);

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

let render c =
  match !current_state with
  | Game g -> render_game c g
  | Title -> render_title_screen c
  | GameOver g ->
      render_game c g;
      Canvas.setFillColor c Color.red;
      Canvas.fillText c "Game Over!" (float_of_int cols *. cell_size /. 2., 100.);
      Canvas.fillText c "Press R to restart"
        (float_of_int cols *. cell_size /. 2., 150.)
  | Pause g ->
      render_game c g;
      Canvas.setFillColor c Color.green;
      Canvas.fillText c "Paused" (float_of_int cols *. cell_size /. 2., 100.);
      Canvas.fillText c "Press P to resume"
        (float_of_int cols *. cell_size /. 2., 150.)
  | About -> ()

let () =
  Backend.init ();

  let width = int_of_float (float_of_int cols *. cell_size) in
  let height = int_of_float (float_of_int rows *. cell_size) in
  let c =
    Canvas.createOnscreen ~title:"Ocamltris" ~pos:(300, 200)
      ~size:(width + 400, height)
      ()
  in
  Canvas.setFont c "Liberation Sans" ~size:30.0 ~slant:Font.Roman
    ~weight:Font.bold;
  Canvas.setLineWidth c 2.;

  let last_tick = ref (Unix.gettimeofday ()) in
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
              | Event.KeyQ -> Backend.stop ()
              | Event.KeyH -> Tetris.shift g (-1)
              | Event.KeyL -> Tetris.shift g 1
              | Event.KeyA -> Tetris.rotate_ccw g
              | Event.KeyD -> Tetris.rotate_cw g
              | Event.KeyC -> Tetris.hold g
              | Event.KeyK -> Tetris.rotate_cw g
              | Event.KeyJ ->
                  ignore (Tetris.tick g);
                  last_tick := Unix.gettimeofday ()
              | Event.KeySpacebar -> Tetris.hard_drop g
              | Event.KeyP -> current_state := Pause g
              | Event.KeyY -> Tetris.add_garbage g 1
              | _ -> ()
            in
            render c
        | Title -> (
            match key with
            | Event.KeySpacebar ->
                let game = Tetris.create (cols, rows) in
                current_state := Game game;
                last_tick := Unix.gettimeofday ();
                render c
            | _ -> ())
        | Pause g -> (
            match key with
            | Event.KeyP ->
                current_state := Game g;
                last_tick := Unix.gettimeofday ();
                render c
            | _ -> ())
        | GameOver g -> (
            match key with
            | Event.KeyR ->
                let game = Tetris.create (cols, rows) in
                current_state := Game game;
                last_tick := Unix.gettimeofday ();
                render c
            | _ -> ())
        | _ -> ())
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
               let delta_time = now -. !last_tick in
               if delta_time >= tick_interval then begin
                 ignore (Tetris.tick g);
                 last_tick := now
               end);
            render c
        | _ -> ())
      Event.frame
  in
  Backend.run (fun () ->
      ignore stop_on_close;
      ignore controls)
