open Core_kernel.Std
open List
open Printf

let abas s =
  let rec inner xs acc = match xs with
    | x :: y :: x' :: _ when x = x' && x <> y ->
        inner (drop xs 1) ((take xs 3 |> String.of_char_list) :: acc)
    | _ :: xs' -> inner xs' acc
    | _ -> acc in
  inner (String.to_list s) []

let flip_aba s = match (String.to_list s) with
  | x :: y :: x' :: _ when x = x' -> String.of_char_list [y; x; y;]
  | _ -> failwith "not an aba!"

let sections s =
  let raw_sections = String.split_on_chars ~on:['[';']'] s in
  let subnets = filteri ~f:(fun i _ -> i land 1 = 0) raw_sections in
  let hypernets = filteri ~f:(fun i _ -> i land 1 = 1) raw_sections in
  subnets, hypernets

let has_ssl l =
  let subnets, hypernets = sections l in
  let subnet_abas = map ~f:abas subnets |> join |> String.Set.of_list in
  let hypernet_abas = map ~f:abas hypernets |> join >>| flip_aba |> String.Set.of_list in
  String.Set.inter subnet_abas hypernet_abas |> String.Set.length |> (<) 0

let go ls = filter ~f:has_ssl ls |> length |> printf "%d\n"

let () = In_channel.read_lines "input.txt" |> go
