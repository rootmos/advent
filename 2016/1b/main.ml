open Core_kernel.Std
open List
open Printf

module S = Set.Make(struct include Tuple.Make(Int)(Int) include Tuple.Comparable(Int)(Int) end)

let taxi_distance (p1, p2) (q1, q2) = abs (p1 - q1) + abs (p2 - q2)
let smult k (x, y) = (k*x, k*y)

let go l =
  let raw_dirs = String.split ~on:',' l |> map ~f:String.strip in
  let ds = [(1, 0); (0, -1); (-1, 0); (0, 1)] in
  let turn di = function
    | 'R' -> (di + 1) % 4
    | 'L' -> (di - 1) % 4
    | _ -> Error.raise (Error.of_string "ops!") in
  let move (di, (x, y), prev) s ~k =
    let di' = (String.to_list s |> hd_exn |> turn di) in
    let l = (String.drop_prefix s 1 |> Int.of_string) in
    let d = nth_exn ds di' in
    let v's = range ~stop:`inclusive 1 l
      |> map ~f:(fun k -> smult k d)
      |> map ~f:(fun (dx, dy) -> (x + dx, y + dy)) in
    Sequence.delayed_fold ~init:prev ~f:(fun prev' v' ~k ->
      printf "(%d,%d)\n" (fst v') (snd v');
      if (S.mem prev' v') then v' else k (S.add prev' v'))
      ~finish:(fun p -> k (di', (last_exn v's), p))
      (Sequence.of_list v's) in
  let (x, y) = Sequence.delayed_fold
    ~init:(3, (0,0), S.empty)
    ~f:move
    ~finish:(fun (_, v, _) -> Error.raise (Error.of_string "reached the end!"))
    (Sequence.of_list raw_dirs) in
  printf "(%d,%d)\n" x y;
  taxi_distance (x, y) (0, 0) |> printf "%d\n"

let () =
  In_channel.with_file "input.txt" ~f:(fun f -> In_channel.input_all f |> go)
