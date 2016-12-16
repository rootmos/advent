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
      let data = (take tail l) in
      let expanded = range 0 n >>| (fun _ -> data) |> concat in
      append expanded (decode @@ drop tail l)
  | None ->
      match cs with
      | c :: cs' -> c :: decode cs'
      | [] -> []

let go ls = ls >>| String.to_list >>| decode >>| String.of_char_list
  >>| (fun s -> printf "%d\n" (String.length s)) |> ignore

let () = In_channel.read_lines "input.txt" |> go
