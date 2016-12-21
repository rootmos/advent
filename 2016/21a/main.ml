open Core_kernel.Std
open Printf
open Re2


type op =
  SwapPos of int * int
| SwapLetter of char * char
| RotateLeft of int
| RotateRight of int
| RotateBasedOn of char
| ReverseRange of int * int
| Move of int * int

let first_ok xs =
  let open List in
  let is_ok = function
    | Ok x -> Some x
    | _ -> None in
  filter_map ~f:is_ok xs |> hd

let parse_ops l =
  let get_int m = Option.value_exn m |> Int.of_string in
  let get_char m = Option.value_exn m |> String.to_list |> List.hd_exn in

  let parses = [
    Or_error.(
      Regex.create_exn "swap position (\\d+) with position (\\d+)"
      |> Fn.flip Regex.find_submatches l
      >>| (fun ms -> SwapPos (get_int ms.(1), get_int ms.(2)))
      );

    Or_error.(
      Regex.create_exn "swap letter ([a-z]) with letter ([a-z])"
      |> Fn.flip Regex.find_submatches l
      >>| (fun ms -> SwapLetter (get_char ms.(1), get_char ms.(2)))
      );

    Or_error.(
      Regex.create_exn "rotate left (\\d+)"
      |> Fn.flip Regex.find_submatches l
      >>| (fun ms -> RotateLeft (get_int ms.(1)))
      );

    Or_error.(
      Regex.create_exn "rotate right (\\d+)"
      |> Fn.flip Regex.find_submatches l
      >>| (fun ms -> RotateRight (get_int ms.(1)))
      );

    Or_error.(
      Regex.create_exn "rotate based on position of letter ([a-z])"
      |> Fn.flip Regex.find_submatches l
      >>| (fun ms -> RotateBasedOn (get_char ms.(1)))
      );

    Or_error.(
      Regex.create_exn "reverse positions (\\d+) through (\\d+)"
      |> Fn.flip Regex.find_submatches l
      >>| (fun ms -> ReverseRange (get_int ms.(1), get_int ms.(2)))
      );

    Or_error.(
      Regex.create_exn "move position (\\d+) to position (\\d+)"
      |> Fn.flip Regex.find_submatches l
      >>| (fun ms -> Move (get_int ms.(1), get_int ms.(2)))
      );
  ] in

  Option.value_exn (first_ok parses)

let apply_op s = function
  | _ -> s

let go ls =
  let open List in
  let ops = ls >>| parse_ops in
  fold ops ~init:"abcde" ~f:apply_op |> printf "%s\n"

let () = In_channel.read_lines "simple.txt" |> go
