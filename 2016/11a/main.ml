open Core_kernel.Std
open Printf

type thing = Microchip of string | Generator of string | Elevator of unit

let example_setup = [
  [Microchip "hydrogen"; Microchip "lithium"; Elevator ()];
  [Generator "hydrogen"];
  [Generator "lithium"];
  []]

let show_thing = function
  | Microchip s -> sprintf "%s microchip" s
  | Generator s -> sprintf "%s generator" s
  | Elevator s -> "elevator"

let show_floors floors =
  let open List in
  let show_floor i floor =
    let things = floor >>| show_thing |> String.concat ~sep:", " in
    printf "F%d: %s\n" (i+1) things in
  iteri floors ~f:show_floor

let () = show_floors example_setup
