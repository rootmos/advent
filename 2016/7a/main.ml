open Core_kernel.Std
open List
open Printf

let has_tls s =
  let rec inner xs = match xs with
    | x :: y :: y' :: x' :: _ when x = x' && y = y' && x <> y -> true
    | _ :: xs' -> inner xs'
    | _ -> false in
  String.to_list s |> inner

let go ls = map ~f:(String.split_on_chars ~on:['[';']']) ls
  >>| mapi ~f:(fun i s ->
    if (i land 1) = 0 then Some (has_tls s)
    else if (has_tls s) then None else Some false)
  |> filter ~f:(fun bs -> Option.all bs |> Option.map ~f:(reduce_exn ~f:(||)) |> (=) (Some true))
  |> length |> printf "%d\n"

let () = In_channel.read_lines "input.txt" |> go
