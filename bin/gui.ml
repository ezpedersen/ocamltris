open OcamlCanvas.V1

let cell_size = 30.
let cols = 10
let rows = 20
let tick_interval = 1.0

let render c game =
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
  let game = Tetris.create (cols, rows) in
  let last_tick = ref (Unix.gettimeofday ()) in
  render c game;
  Canvas.show c;
  let stop_on_close = React.E.map (fun _ -> Backend.stop ()) Event.close in

  let controls =
    React.E.map
      (fun { Event.data = { Event.key; _ }; _ } ->
        let _ =
          match key with
          | Event.KeyQ -> Backend.stop ()
          | Event.KeyH -> Tetris.shift_right game (-1)
          | Event.KeyL -> Tetris.shift_right game 1
          | Event.KeyA -> Tetris.rotate_ccw game
          | Event.KeyD -> Tetris.rotate_cw game
          | Event.KeyC -> Tetris.hold game
          | Event.KeyK -> Tetris.rotate_cw game
          | Event.KeyJ -> ignore (Tetris.tick game)
          | Event.KeySpacebar -> Tetris.hard_drop game
          | _ -> ()
        in
        render c game)
      Event.key_down
  in
  let _ =
    React.E.map
      (fun { Event.timestamp; _ } ->
        let now = Unix.gettimeofday () in
        let delta_time = now -. !last_tick in
        if delta_time >= tick_interval then begin
          ignore (Tetris.tick game);
          last_tick := now;
          render c game
        end)
      Event.frame
  in
  Backend.run (fun () ->
      ignore stop_on_close;
      ignore controls)
