open Core_kernel.Std
open Printf
open Re2

type disk = {
  x: int; y: int;
  size: int;
  used: int;
  target: bool
}

let show_disk disk = sprintf "disk (%d,%d) size: %dT used: %dT" disk.x disk.y disk.size disk.used

let parse_disk l =
  let get_int m = Option.value_exn m |> Int.of_string in

  Or_error.(
    Regex.create_exn "/dev/grid/node-x(\\d+)-y(\\d+)\\s+(\\d+)T\\s+(\\d+)T.*"
    |> Fn.flip Regex.find_submatches l
    >>| (fun ms -> {
      x = get_int ms.(1);
      y = get_int ms.(2);
      size = get_int ms.(3);
      used = get_int ms.(4);
      target = false})
    |> ok_exn)

let avail d = d.size - d.used

let dist a b = abs (a.x - b.x) + abs (a.y - b.y)
let eq a b = dist a b = 0
let is_adjacent a b = dist a b = 1
let is_viable a b = dist a b <> 0 && a.used > 0 && a.used <= avail b

let possible_moves disks =
  let open List in
  fold disks ~init:[] ~f:(fun ps a -> fold disks ~init:ps ~f:(fun ps' b ->
      if is_viable a b && is_adjacent a b then (a, b) :: ps' else ps'))

let move disks (a, b) =
  let open List in
  disks >>| (fun d ->
    if eq a d then { a with used = 0; target = false }
    else if eq b d then { b with used = b.used + a.used; target = a.target }
    else d)

let find disks (x, y) = List.find_exn disks ~f:(fun d -> d.x = x && d.y = y)

let are_we_there_yet disks = find disks (0, 0) |> (fun d -> d.target)

let rec walk depth situations =
  printf "Walked to depth: %d, situations: %d\n" depth (List.length situations);
  if List.find situations ~f:are_we_there_yet |> Option.is_some then depth
  else
    let open List in
    let situations' = situations >>| (fun disks -> possible_moves disks >>| move disks) in
    walk (depth + 1) (concat situations')

let find_target disks =
  let x = List.fold disks ~init:0 ~f:(fun x d -> if d.y = 0 && d.x > x then d.x else x) in
  (x, 0)

let set_target (x, y) disks =
  List.map disks ~f:(fun d -> if d.x = x && d.y = y then { d with target = true } else d)

let go ls =
  let open List in
  let disks = drop ls 2 >>| parse_disk in
  let (x,y) = find_target disks in
  printf "Target data starts on disk (%d, %d)\n" x y;
  let disks' = set_target (x, y) disks in
  let depth = walk 0 [disks'] in
  printf "Moves: %d\n" depth


let () = In_channel.read_lines "simple.txt" |> go
