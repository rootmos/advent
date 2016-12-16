open Core_kernel.Std
open List
open Printf
open Re2

let marker cs =
  match Regex.find_submatches (Regex.create_exn "^\\((\\d+)x(\\d+)\\)(.*)") (String.of_char_list cs) with
  | Ok ms ->
      let l = Option.value_exn ms.(1) |> Int.of_string in
      let n = Option.value_exn ms.(2) |> Int.of_string in
      let tail = Option.value_exn ms.(3) |> String.to_list in
      Some (l, n, tail)
  | _ -> None

let rec decode cs =
  match marker cs with
  | Some (l, n, tail) ->
      let dl = decode (take tail l) in
      dl*n + (drop tail l |> decode)
  | None ->
      match cs with
      | c :: cs' -> 1 + decode cs'
      | [] -> 0

let go ls = ls >>| String.to_list >>| decode >>| printf "%d\n" |> ignore

let () = In_channel.read_lines "input.txt" |> go
