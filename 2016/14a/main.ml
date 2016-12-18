open Core_kernel.Std
open List
open Printf

let md5 s = Digest.string s |> Digest.to_hex

let has_repetition n s =
  let rec inner c m = function
    | c' :: cs when c = c' -> let m' = m + 1 in if m' = n then Some c else inner c m' cs
    | c' :: cs -> inner c' 1 cs
    | [] -> None in
  inner ' ' 1 (String.to_list s)

let make_stream seed =
  let raw_stream start = Sequence.unfold ~init:start ~f:(fun i ->
    let hash = md5 @@ sprintf "%s%d" seed i in
    Some ((i, hash), i + 1)) in

  let has_five_fold_continuation c start = 
    let pattern = sprintf "%c%c%c%c%c" c c c c c in
    Sequence.take (raw_stream start) 1000 |> Sequence.find ~f:(fun (i, s) ->
      String.substr_index s ~pattern:pattern |> Option.is_some) |> Option.is_some in

  Sequence.filter (raw_stream 0) ~f:(fun (i, s) ->
    Option.(has_repetition 3 s >>| (fun c -> has_five_fold_continuation c (i + 1))) |> ((=) (Some true)))

let () =
  let stream = make_stream "qzyelonm" in
  Sequence.(take stream 64 |> iter ~f:(fun (i, s) -> printf "%d %s\n" i s))
