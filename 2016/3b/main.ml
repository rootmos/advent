open Core_kernel.Std
open List
open Printf

let possible = function
  | [a; b; c] when a + b <= c -> false
  | [a; b; c] when b + c <= a -> false
  | [a; b; c] when a + c <= b -> false
  | [a; b; c] -> true
  | _ -> Error.raise (Error.of_string "ops!")

let mangle (l: string): int list =
  String.strip l |> String.split ~on:' ' |> filter ~f:(fun s -> s <> "") |> map ~f:Int.of_string

let go ls =
  let is = map ~f:mangle ls in
  let xs = groupi ~break:(fun i _ _ -> i mod 3 = 0) is |> map ~f:transpose_exn in
  count (concat xs) ~f:possible |> printf "%d\n"

let () =
  In_channel.with_file "input.txt" ~f:(fun f -> In_channel.input_lines f |> go)
