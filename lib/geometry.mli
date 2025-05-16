(*Geometry module that contains functions and type definitions used by multiple
  different files.*)

type point = {
  x : int;
  y : int;
}
(* respresents a point in 2D space *)

(* helper function for creating points *)
val point : int -> int -> point

type fpoint = {
  fx : float;
  fy : float;
}
(** represents a point with floating-point precision in 2D space *)

(* helper function for creating fpoints *)
val fpoint : float -> float -> fpoint

(** The individual piece type enum *)
type piece_type =
  | O
  | I
  | S
  | Z
  | L
  | J
  | T

type piece = {
  piece_type : piece_type;
  rotation : int ref;
  position : fpoint;
}
(** position of "fpoint" of piece, see https://shorturl.at/GEqmK. [piece] is a
    piece in the game. [rotation] is the current rotation of the piece
    (0-3).[position] is the position of the piece in the well. Represents a
    piece being controlled (once in the well its just blocks). Citation:
    https://gamedev.stackexchange.com/questions/208367/how-is-rotation-defined-in-a-tetris-game*)

val string_of_piece_type : piece_type -> string
(** Returns the string of the piece_type *)

val make_piece : piece_type -> int ref -> fpoint -> piece
(** helper function for creating a piece *)

val base_geometry : piece_type -> fpoint list
(** Given a piece type, [base_geometry t] is the points a piece takes up
    relative to its position *)

val rotate_point_90 : fpoint -> fpoint
(** Rotates the point 90 degrees counterclockwise *)

val rotated_geometry : piece -> fpoint list
(** Rotates an entire piece counterclockwise according to its current rotation
    point and returns the list of rotated points. *)

val point_of_fpoint : fpoint -> point
(** Converts an fpoint into a point by flooring all floating-point numbers. *)

val piece_pos : piece -> point list
(** Calculates the list of points in [piece] with respect to the piece's
    absolute position in the tetris game. *)
