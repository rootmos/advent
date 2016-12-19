open Core_kernel.Std
open Printf

type thing = Microchip of string | Generator of string

let example_setup = [
  [Microchip "hydrogen"; Microchip "lithium"];
  [Generator "hydrogen"];
  [Generator "lithium"];
  []]

let generators floor =
  let rec inner acc = function
    | Generator g :: ts -> inner (g :: acc) ts
    | _ :: ts -> inner acc ts
    | [] -> acc in
  inner [] floor

let microchips floor =
  let rec inner acc = function
    | Microchip m :: ts -> inner (m :: acc) ts
    | _ :: ts -> inner acc ts
    | [] -> acc in
  inner [] floor

let is_safe floor =
  let open List in
  let gs = generators floor in
  if (length gs) = 0 then true
  else
    let ms = microchips floor in
    let unpaired_ms = filter ms ~f:(fun m -> not (mem gs m)) in
    (length unpaired_ms) = 0

let show_thing = function
  | Microchip s -> sprintf "%s microchip" s
  | Generator s -> sprintf "%s generator" s

let show_floors elevator floors =
  let open List in
  let show_floor i floor =
    let things = floor >>| show_thing |> String.concat ~sep:", " in
    let elevator_char = if elevator = i then 'E' else ' ' in
    let safe_char = if is_safe floor then ' ' else '!' in
    printf "F%d: (%c) %c %s\n" (i+1) elevator_char safe_char things in
  iteri floors ~f:show_floor;
  printf "\n"

let take_2 xs =
  let open List in
  let rec inner acc = function
    | x :: tail ->
        let acc' = append acc (tail >>| (fun y -> [x; y])) in
        inner acc' tail
    | [] -> acc in
  inner [] xs

let move floors from_floor to_floor things =
  let open List in
  mapi floors ~f:(fun i floor ->
    if i = from_floor then filter floor ~f:(fun t -> not (mem things t))
    else if i = to_floor then concat [floor; things]
    else floor
  )

let surrounding_floors = function
  | 0 -> [1]
  | 1 -> [0; 2]
  | 2 -> [1; 3]
  | 3 -> [2]
  | _ -> failwith "oh noes!"

let all_floors_safe floors = List.for_all

let steps (elevator, floors) =
  let open List in
  let floor = nth_exn floors elevator in
  let things_to_move = concat [(floor >>| (fun t -> [t])); take_2 floor] in
  cartesian_product (surrounding_floors elevator) things_to_move
    >>| (fun (to_floor, things) -> (to_floor, move floors elevator to_floor things))
    |> filter ~f:(fun (_, floors) -> List.for_all floors ~f:is_safe)

let are_we_there_yet (elevator, floors) =
  if elevator <> 3 then false
  else
    let open List in
    let is_empty i = nth_exn floors i |> length |> (=) 0 in
    is_empty 0 && is_empty 1 && is_empty 2

let rec walk n possible_floors =
  let open List in
  if find possible_floors ~f:are_we_there_yet |> Option.is_some then n
  else
    possible_floors >>| steps |> concat |> walk (n+1)

let () =
  let open List in
  show_floors 0 example_setup;
  walk 0 [(0, example_setup)] |> printf "%d\n"

