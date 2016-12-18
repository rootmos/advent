open Core_kernel.Std
open List
open Printf

type tile = Safe | Trap

let get_tile row n = if (n < 0) || (n >= (Array.length row)) then Safe else row.(n)

let make_row s =
  let check = function
    | '.' -> Safe
    | '^' -> Trap
    | _ -> failwith "oh-noes!" in
  String.to_list s >>| check |> to_array

let show_tile = function
  | Safe -> '.'
  | Trap -> '^'

let show_row r = Array.to_list r >>| show_tile |> String.of_char_list

let decide_tile prev_row n =
  let left = get_tile prev_row (n - 1) in
  let center = get_tile prev_row (n) in
  let right = get_tile prev_row (n + 1) in
  match (left, center, right) with
  | (Trap, Trap, Safe) -> Trap
  | (Safe, Trap, Trap) -> Trap
  | (Trap, Safe, Safe) -> Trap
  | (Safe, Safe, Trap) -> Trap
  | _ -> Safe

let next_row prev_row = range 0 (Array.length prev_row) >>| decide_tile prev_row |> to_array

let () =
  let r0 = make_row ".^..^....^....^^.^^.^.^^.^.....^.^..^...^^^^^^.^^^^.^.^^^^^^^.^^^^^..^.^^^.^^..^.^^.^....^.^...^^.^." in
  let rows = Sequence.unfold ~init:r0 ~f:(fun r -> let r' = next_row r in Some (r, r')) in
  let tiles = Sequence.take rows 400000 in

  Sequence.map tiles ~f:(fun r -> Array.count r ~f:((=) Safe)) |> Sequence.reduce_exn ~f:(+) |> printf "%d\n"

