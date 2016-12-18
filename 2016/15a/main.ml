open Core_kernel.Std
open List
open Printf
open Re2

type disc =
  { id : int;
    initial_position : int;
    positions : int
  }

let parse_disc l =
  let get_int m = Option.value_exn m |> Int.of_string in

  let pattern = Regex.create_exn "Disc #(\\d+) has (\\d+) positions; at time=0, it is at position (\\d+)" in
  let ms = Regex.find_submatches pattern l |> Or_error.ok_exn in
  { id = get_int ms.(1); positions = get_int ms.(2); initial_position = get_int ms.(3) }

let is_open t d = (d.initial_position + d.id + t) % d.positions = 0

let go ls =
  let discs = ls >>| parse_disc in
  let times = Sequence.unfold ~init:0 ~f:(fun i -> Some (i, i + 1)) in
  let opens = Sequence.filter times ~f:(fun t -> discs >>| is_open t |> for_all ~f:ident) in
  Sequence.hd_exn opens |> printf "%d\n"


let () = In_channel.read_lines "input.txt" |> go
