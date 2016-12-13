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
    |> filter ~f:((<>) '-')
    |> sort ~cmp:Pervasives.compare
    |> group ~break:(<>)
    >>| (fun xs -> (length xs, hd_exn xs))
    |> sort ~cmp:(fun (i, c) (i', c') ->
        let c1 = (Pervasives.compare i' i) in
        let c2 = (Pervasives.compare c c') in
        if c1 <> 0 then c1 else c2)
    >>| snd
    |> Fn.flip take 5
    |> String.of_char_list

let shift i = function
  | '-' -> ' '
  | c -> "abcdefghijklmnopqrstuvwxyz".[(Char.to_int c - 97 + i) % 26]

let decode s i = String.to_list s >>| shift i |> String.of_char_list

let go ls =
  map ~f:split_line ls
    |> filter ~f:(fun (n, _, c) -> checksum n = c)
    >>| (fun (n, s, _) -> s, decode n s)
    |> filter ~f:(fun (_, n) -> Regex.matches (Regex.create_exn "object") n)
    >>| (fun (s, n) -> printf "%d %s\n" s n)
    |> ignore

let () = In_channel.read_lines "input.txt" |> go
