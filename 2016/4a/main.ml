open Core_kernel.Std
open List
open Printf
open Re2

let split_line l =
  let p = Regex.create_exn "([a-z-]+)-(\\d+)\\[(\\w+)]" in
  let ms: string option array = Regex.find_submatches p l |> Or_error.ok_exn in
  let name = Option.value_exn (Array.get ms 1) in
  let section = Option.value_exn (Array.get ms 2) |> Int.of_string in
  let chk = Option.value_exn (Array.get ms 3) in
  (name, section, chk)

let checksum s =
  String.to_list s
    |> filter ~f:(fun c -> c <> '-')
    |> sort ~cmp:Pervasives.compare
    |> group ~break:(<>)
    |> map ~f:(fun xs -> (length xs, hd_exn xs))
    |> sort ~cmp:(fun (i, c) (i', c') ->
        let c1 = (Pervasives.compare i' i) in
        let c2 = (Pervasives.compare c c') in
        if c1 <> 0 then c1 else c2)
    |> map ~f:(fun (l, c) -> printf "(%d, %c)\n" l c; (l, c))
    |> map ~f:snd
    |> (fun l -> take l 5)
    |> String.of_char_list
    |> (fun x -> printf "%s\n" x; x)

let go ls =
  map ~f:split_line ls
    |> filter ~f:(fun (n, _, c) -> checksum n = c)
    |> map ~f:(fun (_, s, _) -> s)
    |> fold ~init:0 ~f:(+)
    |> printf "%d\n"

let () = In_channel.with_file "input.txt" ~f:(fun f -> In_channel.input_lines f |> go)
