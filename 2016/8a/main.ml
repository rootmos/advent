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

let xdim = 7
let ydim = 3

let apply_cmd m = function
  | RotX (x, i) ->
      Array.mapi m ~f:(fun y' row -> Array.mapi row ~f:(fun x' v ->
        if x = x' then m.((y'-i) % ydim).(x')
        else v))
  | RotY (y, i) ->
      Array.mapi m ~f:(fun y' row ->
        if y = y' then Array.mapi row ~f:(fun x' v -> m.(y).((x' - i) % xdim))
        else row)
  | Rect (x, y) -> Array.mapi m ~f:(fun y' row -> Array.mapi row ~f:(fun x' v ->
      if (x' < x) && (y' < y) then true else v))

let print_matrix m = Array.iter ~f:(fun row -> Array.iter ~f:(fun cell -> printf @@ if cell then "#" else ".") row; printf "\n") m

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

let go ls =
  let cmds = ls >>| parse_cmd in
  let cells = Array.make_matrix ~dimx:ydim ~dimy:xdim false in
  let cells' = fold ~init:cells ~f:(fun m c ->
    let m' = apply_cmd m c in
    print_matrix m';
    printf "\n";
    m') cmds in
  printf "\n------\n\n";
  print_matrix cells';
  let lit = Array.(map cells' ~f:(fun row -> map row ~f:Bool.to_int |> reduce_exn ~f:(+)) |> reduce_exn ~f:(+)) in
  printf "lit cells: %d\n" lit

let () = In_channel.read_lines "simple.txt" |> go
