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
  | (-1,  1) -> 1
  | ( 0,  1) -> 2
  | ( 1,  1) -> 3
  | (-1,  0) -> 4
  | ( 0,  0) -> 5
  | ( 1,  0) -> 6
  | (-1, -1) -> 7
  | ( 0, -1) -> 8
  | ( 1, -1) -> 9
  | _ -> Error.raise (Error.of_string "ops!")

let inbounds (x, y) = abs(x) <= 1 && abs(y) <= 1

let tsum (x1, y1) (x2, y2) =
  let s = (x1 + x2, y1 + y2) in
  if inbounds s then s else (x1, y1)

let go ls =
  fold ~init:(0,0) ~f:(fun v l ->
    let (x,y) = fold ~init:v ~f:(fun w c -> turn c |> tsum w) (String.to_list l) in
    button_translator (x, y) |> printf "%d";
    (x, y)
  ) ls;
  printf "\n"

let () =
  In_channel.with_file "input.txt" ~f:(fun f -> In_channel.input_lines f |> go)
