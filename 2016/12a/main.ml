open Core_kernel.Std
open List
open Printf
open Re2

type register = Register of char * int option

type op =
  Insert of int * char
| Copy of char * char
| Incr of char
| Decr of char
| JumpRegister of char * int
| JumpInteger of int * int

let first_ok xs =
  let is_ok = function
    | Ok x -> Some x
    | _ -> None in
  filter_map ~f:is_ok xs |> hd

let parse_op l =
  let get_int m = Option.value_exn m |> Int.of_string in
  let get_register m = Option.value_exn m |> (fun s -> s.[0]) in

  let insert = Or_error.(
    (Regex.find_submatches (Regex.create_exn "cpy ([-]*\\d+) (\\w)") l
      >>| (fun ms -> Insert (get_int ms.(1), get_register ms.(2))))) in

  let copy = Or_error.(
    (Regex.find_submatches (Regex.create_exn "cpy (\\w) (\\w)") l
      >>| (fun ms -> Copy (get_register ms.(1), get_register ms.(2))))) in

  let incr = Or_error.(
    (Regex.find_submatches (Regex.create_exn "inc (\\w)") l
      >>| (fun ms -> Incr (get_register ms.(1))))) in

  let decr = Or_error.(
    (Regex.find_submatches (Regex.create_exn "dec (\\w)") l
      >>| (fun ms -> Decr (get_register ms.(1))))) in

  let jump_register = Or_error.(
    (Regex.find_submatches (Regex.create_exn "jnz ([a-z]) ([-]*\\d+)") l
      >>| (fun ms -> JumpRegister (get_register ms.(1), get_int ms.(2))))) in

  let jump_int = Or_error.(
    (Regex.find_submatches (Regex.create_exn "jnz ([-]*\\d+) ([-]*\\d+)") l
      >>| (fun ms -> JumpInteger (get_int ms.(1), get_int ms.(2))))) in

  Option.value_exn (first_ok [insert; copy; incr; decr; jump_register; jump_int])


let read rs r =
  match Char.Map.find rs r with
  | Some i -> i
  | None -> 0

let write rs r i = Char.Map.add rs ~key:r ~data:i

let empty_registers = Char.Map.empty

let show_registers rs = Char.Map.iteri rs ~f:(fun ~key ~data -> printf "%c: %d\n" key data)

let rec exec_op ops rs ip =
  if ip >= Array.length ops then rs
  else let ip', rs' =
    match ops.(ip) with
    | Insert (i, r) -> ip + 1, write rs r i
    | Copy (r, r') -> ip + 1, read rs r |> write rs r'
    | Incr r -> ip + 1, read rs r |> (fun i -> write rs r (i+1))
    | Decr r -> ip + 1, read rs r |> (fun i -> write rs r (i-1))
    | JumpInteger (n, i) -> if n <> 0 then (ip + i, rs) else (ip + 1, rs)
    | JumpRegister (r, i) -> if (read rs r) <> 0 then (ip + i, rs) else (ip + 1, rs) in
  exec_op ops rs' ip'

let go ls =
  let ops = ls >>| parse_op |> to_array in
  let rs = exec_op ops empty_registers 0 in
  show_registers rs


let () = In_channel.read_lines "input_b.txt" |> go
