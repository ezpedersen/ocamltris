open OcamlCanvas.V1

let cell_size = 20.
let cols = 10
let rows = 20

let draw_grid c =
  Canvas.setStrokeColor c Color.black;
  Canvas.setLineWidth c 1.0;
  for row = 0 to rows - 1 do
    for col = 0 to cols - 1 do
      let x = float_of_int col *. cell_size in
      let y = float_of_int row *. cell_size in
      Canvas.strokeRect c ~pos:(x, y) ~size:(cell_size, cell_size)
    done
  done

let () =
  Backend.init ();

  let width = int_of_float (float_of_int cols *. cell_size) in
  let height = int_of_float (float_of_int rows *. cell_size) in

  let c =
    Canvas.createOnscreen ~title:"Ocamltris" ~pos:(300, 200)
      ~size:(width, height) ()
  in

  Canvas.setFillColor c Color.white;
  Canvas.fillRect c ~pos:(0.0, 0.0)
    ~size:(float_of_int width, float_of_int height);

  draw_grid c;

  Canvas.show c;

  let stop_on_close = React.E.map (fun _ -> Backend.stop ()) Event.close in

  let stop_on_escape =
    React.E.map
      (fun { Event.data = { Event.key; _ }; _ } ->
        if key = KeyEscape then Backend.stop ())
      Event.key_down
  in

  Backend.run (fun () ->
      ignore stop_on_close;
      ignore stop_on_escape)
