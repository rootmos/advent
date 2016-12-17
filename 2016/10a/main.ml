open Core_kernel.Std
open List
open Printf
open Re2

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

type bot_state = BotState of bot * int option * int option

let show_mi = function
  | Some i -> sprintf "Some %d" i
  | None -> "None"

let show_bot_state (BotState (b, ml, mh)) =
  sprintf "state bot:(%s) low:(%s) high:(%s)" (show_bot b) (show_mi ml) (show_mi mh)

let bot_id (Bot (i, _, _)) = i
let bot_state_id (BotState (b, _, _)) = bot_id b

let initial_state cmds =
  let rec inner acc = function
    | AssignInput _ :: tail -> inner acc tail
    | AssignBot b :: tail ->
        let acc' = Int.Map.add ~key:(bot_id b) ~data:(BotState (b, None, None)) acc in
        inner acc' tail
    | [] -> acc in
  inner Int.Map.empty cmds

let show_state state =
  printf "State:\n";
  Int.Map.iteri state ~f:(fun ~key ~data -> printf "%s\n" @@ show_bot_state data)

let give_bot_state v = function
  | BotState (b, Some v', None) when v < v' -> BotState (b, Some v, Some v')
  | BotState (b, Some v', None) when v' < v -> BotState (b, Some v', Some v)
  | BotState (b, None, None) -> BotState (b, Some v, None)
  | s -> failwith (sprintf "can't give value %d to bot_state:(%s)" v (show_bot_state s))

let apply_cmd state cmd =
  match cmd with
  | AssignInput (v, b_id) ->
      Int.Map.change state b_id ~f:(fun mbs -> Option.value_exn mbs |> give_bot_state v |> Option.some)
  | _ -> state

let rec handle_transfer (state, outputs) = function
  | BotState (Bot (_, low, high), Some v, Some v') ->
      let give (st, os) w = function
        | ToOutput o -> st, Int.Map.add os ~key:o ~data:w
        | ToBot b_id ->
            match Int.Map.find st b_id with
            | None -> failwith (sprintf "no bot with id %d" b_id)
            | Some bs ->
                let bs' = give_bot_state w bs in
                let st' = Int.Map.add st ~key:b_id ~data:bs' in
                handle_transfer (st', os) bs' in
      let st', os' = give (state, outputs) v low in
      let st'', os'' = give (st', os') v' high in
      st'', os''
  | _ -> state, outputs

let go ls =
  let cmds = ls >>| parse_cmd in
  cmds >>| (fun c -> printf "%s\n" (show_cmd c)) |> ignore;

  let state = initial_state cmds in
  show_state state;

  let state' = fold cmds ~init:state ~f:(fun st cmd ->
    let st' = apply_cmd st cmd in
    show_state st'; st'
  ) in

  let state'', outputs = Int.Map.fold state' ~init:(state', Int.Map.empty) ~f:(fun ~key ~data acc -> handle_transfer acc data) in
  show_state state'';
  Int.Map.iteri outputs ~f:(fun ~key ~data -> printf "output %d has value %d\n" key data)

let () = In_channel.read_lines "input.txt" |> go
