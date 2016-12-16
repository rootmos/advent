open Core_kernel.Std
open List
open Printf
open Re2

(*let value_to_bot = Int.Table.create ()*)
(*let bots = Int.Table.create ()*)

type gives_to = ToBot of int | ToOutput of int

let show_gives_to = function
  | ToBot i -> sprintf "gives to bot %d" i
  | ToOutput i -> sprintf "gives to output %d" i

type bot = Bot of int * gives_to * gives_to
let show_bot (Bot (i, low, high)) =
  sprintf "bot %d low: (%s) high: (%s)" i (show_gives_to low) (show_gives_to high)

type cmd = AssignInput of int * int | AssignBot of bot

let show_cmd = function
  | AssignInput (i, b) -> sprintf "value %d goes to bot %d" i b
  | AssignBot b -> sprintf "define: %s" (show_bot b)

let first_ok xs =
  let is_ok = function
    | Ok x -> Some x
    | _ -> None in
  filter_map ~f:is_ok xs |> hd


let parse_cmd l =
  let assign_input_pattern = Regex.create_exn "value (\\d+) goes to bot (\\d+)" in
  let assign_input = Or_error.((Regex.find_submatches assign_input_pattern l)
    >>| (fun ms -> AssignInput (
      Option.value_exn ms.(1) |> Int.of_string,
      Option.value_exn ms.(2) |> Int.of_string))) in

  let decide_gives_to i = function
    | "bot" -> ToBot i
    | "output" -> ToOutput i
    | _ -> failwith "unknow target yo" in

  let assign_bot_pattern = Regex.create_exn "bot (\\d+) gives low to (\\w+) (\\d+) and high to (\\w+) (\\d+)" in
  let assign_bot = Or_error.((Regex.find_submatches assign_bot_pattern l)
    >>| (fun ms -> AssignBot (Bot (
      Option.value_exn ms.(1) |> Int.of_string,
      Option.value_exn ms.(2) |> decide_gives_to (Option.value_exn ms.(3) |> Int.of_string),
      Option.value_exn ms.(4) |> decide_gives_to (Option.value_exn ms.(5) |> Int.of_string))))) in

  Option.value_exn (first_ok [assign_input; assign_bot])

let go ls =
  let cmds = ls >>| parse_cmd in
  cmds >>| (fun c -> printf "%s\n" (show_cmd c)) |> ignore

let () = In_channel.read_lines "simple.txt" |> go
