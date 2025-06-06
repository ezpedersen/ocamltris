type point = {
  x : int;
  y : int;
}

let point x y = { x; y }

type fpoint = {
  fx : float;
  fy : float;
}

let fpoint fx fy = { fx; fy }

type piece_type =
  | O
  | I
  | S
  | Z
  | L
  | J
  | T

let string_of_piece_type = function
  | O -> "O"
  | I -> "I"
  | S -> "S"
  | Z -> "Z"
  | L -> "L"
  | J -> "J"
  | T -> "T"
(* let piece_type_of_string = function | "O" -> O | "I" -> I | "S" -> S | "Z" ->
   Z | "L" -> L | "J" -> J | "T" -> T | _ -> failwith "Invalid piece type" *)

type piece = {
  piece_type : piece_type;
  rotation : int ref;
  position : fpoint;
      (* position of "fpoint" of piece, see https://shorturl.at/GEqmK *)
}

let make_piece piece_type rotation position = { piece_type; rotation; position }

let base_geometry = function
  | Z -> [ fpoint 0. 0.; fpoint (-1.) 0.; fpoint 0. 1.; fpoint 1. 1. ]
  | I ->
      [ fpoint 0.5 0.5; fpoint 1.5 0.5; fpoint (-1.5) 0.5; fpoint (-0.5) 0.5 ]
  | J -> [ fpoint 0. 0.; fpoint 1. 0.; fpoint (-1.) 0.; fpoint 1. 1. ]
  | L -> [ fpoint 0. 0.; fpoint 1. 0.; fpoint (-1.) 0.; fpoint (-1.) 1. ]
  | O ->
      [
        fpoint 0.5 0.5;
        fpoint 0.5 (-0.5);
        fpoint (-0.5) 0.5;
        fpoint (-0.5) (-0.5);
      ]
  | T -> [ fpoint 0. 0.; fpoint 0. 1.; fpoint 1. 1.; fpoint (-1.) 1. ]
  | S -> [ fpoint 0. 0.; fpoint 0. 1.; fpoint (-1.) 1.; fpoint 1. 0. ]

let rotate_point_90 offset = fpoint offset.fy (-.offset.fx)

let rotated_geometry p =
  let rec rotation_function n x =
    if n = 0 then x else rotate_point_90 x |> rotation_function (n - 1)
  in
  List.map (rotation_function !(p.rotation)) (base_geometry p.piece_type)

let point_of_fpoint a =
  point (int_of_float (floor a.fx)) (a.fy |> floor |> int_of_float)

let piece_pos p =
  List.map
    (fun x ->
      { fx = p.position.fx +. x.fx; fy = p.position.fy +. x.fy }
      |> point_of_fpoint)
    (rotated_geometry p)
