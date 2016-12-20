open Core_kernel.Std
open List
open Printf
open Re2

let parse_range l =
  let get_int m = Option.value_exn m |> Int.of_string in

  let pattern = Regex.create_exn "(\\d+)-(\\d+)" in
  let ms = Regex.find_submatches pattern l |> Or_error.ok_exn in
  (get_int ms.(1), get_int ms.(2))

let max = 4294967295

let rec walk acc e = function
  | (i, j) :: tail when i > (e + 1) -> let gap = i - (e + 1) in walk (gap + acc) j tail
  | (i, j) :: tail when j > e -> walk acc j tail
  | (i, j) :: tail -> walk acc e tail
  | [] -> if e < max then acc + (max - e) else acc

let go ls =
  let ranges = ls >>| parse_range |> sort ~cmp:Pervasives.compare in
  ranges >>| (fun (i,j) -> printf "%d-%d\n" i j) |> ignore;
  walk 0 0 ranges |> printf "Unblocked: %d\n"

let () = In_channel.read_lines "input.txt" |> go
