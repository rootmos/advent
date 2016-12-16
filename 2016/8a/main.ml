open Core_kernel.Std
open List
open Printf
open Re2

type cmd = | RotX of int * int | RotY of int * int | Rect of int * int

let show_cmd = function
  | RotX (x, i) -> sprintf "rotx %d by %d" x i
  | RotY (y, i) -> sprintf "roty %d by %d" y i
  | Rect (x, y) -> sprintf "rect %dx%d" x y

let first_ok xs =
  let is_ok = function
    | Ok x -> Some x
    | _ -> None in
  filter_map ~f:is_ok xs |> hd

let parse_cmd l =
  let rotx_pattern = Regex.create_exn "rotate column x=(\\d+) by (\\d+)" in
  let rotx = Or_error.((Regex.find_submatches rotx_pattern l)
    >>| (fun ms -> RotX (
      Option.value_exn ms.(1) |> Int.of_string,
      Option.value_exn ms.(2) |> Int.of_string))) in

  let roty_pattern = Regex.create_exn "rotate row y=(\\d+) by (\\d+)" in
  let roty = Or_error.((Regex.find_submatches roty_pattern l)
    >>| (fun ms -> RotY (
      Option.value_exn ms.(1) |> Int.of_string,
      Option.value_exn ms.(2) |> Int.of_string))) in

  let rect_pattern = Regex.create_exn "rect (\\d+)x(\\d+)" in
  let rect = Or_error.((Regex.find_submatches rect_pattern l)
    >>| (fun ms -> Rect (
      Option.value_exn ms.(1) |> Int.of_string,
      Option.value_exn ms.(2) |> Int.of_string))) in

  Option.value_exn (first_ok [rotx; roty; rect])

let go ls = ls >>| parse_cmd >>| show_cmd >>| printf "%s\n" |> ignore

let () = In_channel.read_lines "simple.txt" |> go
