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

let rec apply_op s = function
  | SwapPos (i, j) ->
      String.to_array s
        |> (fun cs -> Array.swap cs i j; cs)
        |> Array.to_list
        |> String.of_char_list
  | SwapLetter (c, c') ->
      let swapper = function
        | d when d = c -> c'
        | d when d = c' -> c
        | d -> d in
      String.map s ~f:swapper
  | RotateLeft n ->
      let rotate_left dl =
        let hd = Option.value_exn (Doubly_linked.first_elt dl) in
        Doubly_linked.move_to_back dl hd; dl in
      let dl = String.to_list s |> Doubly_linked.of_list in
      Fn.apply_n_times ~n:n rotate_left dl
        |> Doubly_linked.to_list |> String.of_char_list
  | RotateRight n ->
      let rotate_right dl =
        let last = Option.value_exn (Doubly_linked.last_elt dl) in
        Doubly_linked.move_to_front dl last; dl in
      let dl = String.to_list s |> Doubly_linked.of_list in
      Fn.apply_n_times ~n:n rotate_right dl
        |> Doubly_linked.to_list |> String.of_char_list
  | ReverseRange (i, j) ->
      let cs = String.to_list s in
      let length = List.length cs in

      let left = if i > 0 then List.slice cs 0 i else [] in
      let center = List.slice cs i (j + 1) in
      let right =  List.slice cs (j + 1) length in

      let center' = Array.of_list center |> (fun a -> Array.rev_inplace a; a) |> Array.to_list in

      List.concat [left; center'; right] |> String.of_char_list
  | RotateBasedOn c ->
      let cs = String.to_list s in
      let i = (Option.value_exn (List.findi cs ~f:(fun _ c' -> c = c'))) |> fst in
      let n = 1 + if i >= 4 then i + 1 else i in
      apply_op s (RotateRight n)
  | Move (i, j) when i < j ->
      let cs = String.to_list s in
      let c = List.nth_exn cs i in
      let cs' = List.mapi cs ~f:(fun i' d ->
        if i = i' then []
        else if j = i' then [d; c]
        else [d]) in
      String.of_char_list (List.concat cs')
  | Move (i, j) when i > j ->
      let cs = String.to_list s in
      let c = List.nth_exn cs i in
      let cs' = List.mapi cs ~f:(fun i' d ->
        if i = i' then []
        else if j = i' then [c; d]
        else [d]) in
      String.of_char_list (List.concat cs')
  | _ -> s

let apply_inverse_op s = function
  | RotateLeft n -> apply_op s (RotateRight n)
  | RotateRight n -> apply_op s (RotateLeft n)
  | Move (i, j) -> apply_op s (Move (j, i))
  | RotateBasedOn c ->
      let open List in
      let left_rotations = range 0 (String.length s)
        >>| (fun n -> apply_op s (RotateLeft n)) in
      find_exn left_rotations ~f:(fun lr -> s = (apply_op lr (RotateBasedOn c)))
  | op -> apply_op s op

let go ls =
  let open List in
  let ops = ls >>| parse_ops |> rev in
  fold ops ~init:"fbgdceah" ~f:(fun s op ->
    let s' = apply_inverse_op s op in printf "%s -> %s\n" s s'; s') |> printf "%s\n"

let () = In_channel.read_lines "input.txt" |> go
