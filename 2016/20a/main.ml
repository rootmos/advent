open Core_kernel.Std
open List
open Printf
open Re2

let parse_range l =
  let get_int m = Option.value_exn m |> Int.of_string in

  let pattern = Regex.create_exn "(\\d+)-(\\d+)" in
  let ms = Regex.find_submatches pattern l |> Or_error.ok_exn in
  (get_int ms.(1), get_int ms.(2))

let rec walk e = function
  | (i, _) :: _ when i > (e + 1) -> e + 1
  | (i, j) :: tail -> walk j tail
  | [] -> failwith "no gaps!"

let go ls =
  let ranges = ls >>| parse_range |> sort ~cmp:Pervasives.compare in
  ranges >>| (fun (i,j) -> printf "%d-%d\n" i j) |> ignore;
  walk 0 ranges |> printf "First gap: %d\n"

let () = In_channel.read_lines "input.txt" |> go
