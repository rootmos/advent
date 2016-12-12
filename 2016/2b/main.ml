open Core_kernel.Std
open List
open Printf

let turn = function
  | 'R' -> (1, 0)
  | 'L' -> (-1, 0)
  | 'U' -> (0, 1)
  | 'D' -> (0, -1)
  | _ -> Error.raise (Error.of_string "ops!")

let button_translator = function
  | ( 0,  2) -> '1'

  | (-1,  1) -> '2'
  | ( 0,  1) -> '3'
  | ( 1,  1) -> '4'

  | (-2,  0) -> '5'
  | (-1,  0) -> '6'
  | ( 0,  0) -> '7'
  | ( 1,  0) -> '8'
  | ( 2,  0) -> '9'

  | (-1, -1) -> 'A'
  | ( 0, -1) -> 'B'
  | ( 1, -1) -> 'C'

  | ( 0, -2) -> 'D'
  | _ -> Error.raise (Error.of_string "ops!")

let inbounds (x, y) = (abs(x) + abs(y)) < 3

let tsum (x1, y1) (x2, y2) =
  let s = (x1 + x2, y1 + y2) in
  if inbounds s then s else (x1, y1)

let go ls =
  fold ~init:(-2,0) ~f:(fun v l ->
    let (x,y) = fold ~init:v ~f:(fun w c -> turn c |> tsum w) (String.to_list l) in
    button_translator (x, y) |> printf "%c";
    (x, y)
  ) ls;
  printf "\n"

let () =
  In_channel.with_file "input.txt" ~f:(fun f -> In_channel.input_lines f |> go)
