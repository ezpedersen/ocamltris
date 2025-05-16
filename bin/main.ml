open Bogue
open Tsdl
module W = Widget
module L = Layout
module T = Trigger

let () = Random.self_init ()
let background_image = "static/bg.png"
let text size str = W.label ~size ~fg:Draw.(opaque black) str

let controls_window () =
  let title = W.label ~size:32 "CONTROLS" in
  let title_layout = L.resident title in
  L.sety title_layout 30;

  let section_title s = text 20 s |> L.resident in
  let control_text s = text 18 s |> L.resident in

  let left_col =
    L.tower
      [
        section_title "Player 1:";
        control_text "A/D: Move Left/Right";
        control_text "W/S: Rotate CW/CCW";
        control_text "Z: Soft Drop";
        control_text "X: Hard Drop";
        control_text "C: Hold";
      ]
  in

  let right_col =
    L.tower
      [
        section_title "Player 2:";
        control_text "J/L: Move Left/Right";
        control_text "I/K: Rotate CW/CCW";
        control_text "M: Soft Drop";
        control_text ". (period): Hard Drop";
        control_text ", (comma): Hold";
      ]
  in

  let general_controls =
    L.tower [ control_text "P: Pause"; control_text "Q: Quit" ]
  in

  let all_controls =
    L.tower ~sep:40
      [
        title_layout;
        L.flat ~align:Draw.Center ~sep:100 [ left_col; right_col ];
        general_controls;
      ]
  in

  let win = Window.create all_controls in
  let board = Main.create [ win ] in
  Main.run board

let bot_settings_window () =
  let title = W.label ~size:32 "BOT SETTINGS" in
  let title_layout = L.resident title in
  L.sety title_layout 30;

  let info1 =
    text 20 "Press 1–4 to select bot difficulty to face off against"
    |> L.resident
  in
  L.sety info1 80;

  let info2 =
    text 20 "Press 6 to watch the max difficulty bot play by itself!"
    |> L.resident
  in
  L.sety info2 120;

  let info3 =
    text 20 "Press 7 to watch two max difficulty bots play each other."
    |> L.resident
  in
  L.sety info3 150;

  let layout = L.tower ~sep:20 [ title_layout; info1; info2 ] in
  let win = Window.create layout in
  let board = Main.create [ win ] in
  Main.run board

let open_new_window title content =
  let label = W.label content in
  let layout = L.resident label in
  let win = Window.create layout in
  let board = Main.create [ win ] in
  Main.run board

let make_button label callback =
  let b = W.button label in
  W.on_click b ~click:(fun _ -> callback ());
  b

let main () =
  let bg = W.image ~w:800 ~h:600 background_image in
  let bg_room = L.resident bg in

  let single = make_button "Single Player" (fun () -> Gui.singleplayer ()) in
  let multi = make_button "2 Player" (fun () -> Gui.multiplayer ()) in
  let bots = make_button "Bots" (fun () -> bot_settings_window ()) in
  let controls = make_button "Controls" (fun () -> controls_window ()) in

  let buttons =
    L.flat ~align:Draw.Center ~sep:40
      (List.map L.resident [ single; multi; bots; controls ])
  in
  L.sety buttons 500;

  let layout = L.superpose [ bg_room; buttons ] in
  let board = Main.of_layout layout in
  Main.run board

let () = main ()
