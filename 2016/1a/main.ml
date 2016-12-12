open Core_kernel.Std
open List
open Printf

let taxi_distance (p1, p2) (q1, q2) = abs (p1 - q1) + abs (p2 - q2)

let go l =
  let raw_dirs = String.split ~on:',' l |> map ~f:String.strip in
  let ds = [(1, 0); (0, -1); (-1, 0); (0, 1)] in
  let smult k (x, y) = (k*x, k*y) in
  let turn di = function
    | 'R' -> (di + 1) % 4
    | 'L' -> (di - 1) % 4
    | _ -> Error.raise (Error.of_string "ops!") in
  let move (di, (x, y)) s =
    let di' = (String.to_list s |> hd_exn |> turn di) in
    let l = (String.drop_prefix s 1 |> Int.of_string) in
    let (dx, dy) = smult l (nth_exn ds di') in
    let v' = (x + dx, y + dy) in
    (di', v') in
  let (_, (x, y)) = fold ~init:(3, (0,0)) ~f:move raw_dirs in
  printf "(%d,%d)\n" x y;
  taxi_distance (x, y) (0, 0) |> printf "%d\n"

let () = 
  In_channel.with_file "input.txt" ~f:(fun f -> In_channel.input_all f |> go)
