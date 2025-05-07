open OcamlCanvas.V1

let cell_size = 30.
let cols = 10
let rows = 20
let tick_interval = 1.0

let render c game =
  Canvas.setFillColor c Color.white;
  Canvas.fillRect c ~pos:(0., 0.)
    ~size:(float_of_int cols *. cell_size, float_of_int rows *. cell_size);

  for i = 0 to rows - 1 do
    for j = 0 to cols - 1 do
      let entry = Tetris.get_entry game (j, i) in
      let color =
        match entry with
        | 0 -> Color.green
        | 1 -> Color.blue
        | 2 -> Color.red
        | _ -> Color.black
      in
      let x = float_of_int j *. cell_size in
      let y = float_of_int i *. cell_size in
      Canvas.setFillColor c color;
      Canvas.fillRect c ~pos:(x, y) ~size:(cell_size, cell_size);
      Canvas.setStrokeColor c Color.black;
      Canvas.strokeRect c ~pos:(x, y) ~size:(cell_size, cell_size)
    done
  done

let () =
  Backend.init ();
  let width = int_of_float (float_of_int cols *. cell_size) in
  let height = int_of_float (float_of_int rows *. cell_size) in
  let c =
    Canvas.createOnscreen ~title:"Ocamltris" ~pos:(300, 200)
      ~size:(width + 400, height)
      ()
  in
  Canvas.setFillColor c Color.black;
  Canvas.fillRect c ~pos:(0., 0.)
    ~size:(float_of_int (width + 400), float_of_int height);
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
