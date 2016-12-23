open Core_kernel.Std
open Printf
open Re2

module M = Map.Make(struct include Tuple.Make(Int)(Int) include Tuple.Comparable(Int)(Int) end)

type disk = {
  x: int; y: int;
  size: int;
  used: int;
  target: bool
}

let to_hash d = sprintf "%d%d%d%d%d" d.x d.y d.size d.used (Bool.to_int d.target)
let disks_to_hash disks = let open List in M.data disks >>| to_hash |> String.concat

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
let is_viable a b = is_adjacent a b && a.used > 0 && a.used <= avail b

let possible_moves disks =
  M.fold disks ~init:[] ~f:(fun ~key ~data:a ps -> M.fold disks ~init:ps ~f:(fun ~key ~data:b ps' ->
    if is_viable a b then (a, b) :: ps' else ps'))

let possible_moves_involving disks d =
  let open Option in
  let n = M.find disks (d.x, d.y - 1) in
  let s = M.find disks (d.x, d.y + 1) in
  let e = M.find disks (d.x - 1, d.y) in
  let w = M.find disks (d.x + 1, d.y) in

  let moves = [
    n >>= (fun n' -> if is_viable d n' then Some (d, n') else None);
    n >>= (fun n' -> if is_viable n' d then Some (n', d) else None);
    s >>= (fun s' -> if is_viable d s' then Some (d, s') else None);
    s >>= (fun s' -> if is_viable s' d then Some (s', d) else None);
    e >>= (fun e' -> if is_viable d e' then Some (d, e') else None);
    e >>= (fun e' -> if is_viable e' d then Some (e', d) else None);
    w >>= (fun w' -> if is_viable d w' then Some (d, w') else None);
    w >>= (fun w' -> if is_viable w' d then Some (w', d) else None)] in

  List.filter moves ~f:Option.is_some |> Option.all |> (fun x -> Option.value_exn x)

let move disks (a, b) =
  let disks' = M.change disks (a.x, a.y) ~f:(fun _ -> Some { a with used = 0; target = false }) in
  M.change disks' (b.x, b.y) ~f:(fun _ -> Some { b with used = b.used + a.used; target = a.target })

let find_disk disks (x, y) = M.find_exn disks (x, y)


let visited = String.Hash_set.create ()

let find_target disks = List.find_exn (M.data disks) ~f:(fun d -> d.target)

type situation = {
  moves: (disk * disk) list;
  disks: disk M.t
}

let are_we_there_yet s = find_disk s.disks (0, 0) |> (fun d -> d.target)

let next_situations s =
  let open List in
  s.moves >>| (fun (a, b) ->
    let disks' = move s.disks (a, b) in
    let refresh d = find_disk disks' (d.x, d.y) in
    let moves' = concat [
      possible_moves_involving disks' (refresh a);
      possible_moves_involving disks' (refresh b);
      (s.moves |> filter ~f:(fun (a', b') ->
        ((min_elt ~cmp:Pervasives.compare [dist a a'; dist a b'; dist b a'; dist b b']
          |> (fun o -> Option.value_exn o)) > 0) || is_viable (refresh a') (refresh b')))] in
    { moves = moves'; disks = disks' })

let rec walk depth situations =
  printf "Walked to depth: %d, situations: %d, visited: %d\n" depth (List.length situations) (Hash_set.length visited);
  flush_all ();
  if List.find situations ~f:are_we_there_yet |> Option.is_some then depth
  else if (List.length situations) = 0 then failwith "nothing to try!"
  else
    let open List in
    iter situations ~f:(fun s -> Hash_set.add visited (disks_to_hash s.disks));

    let situations' = situations >>| next_situations |> concat
      |> filter ~f:(fun s -> not (Hash_set.mem visited (disks_to_hash s.disks))) in
    walk (depth + 1) situations'

let find_target disks =
  let x = M.fold disks ~init:0 ~f:(fun ~key:(x', y') ~data x -> if y' = 0 then (max x x') else x) in
  (x, 0)

let set_target (x, y) disks =
  M.change disks (x, y) ~f:(fun maybe_disk -> Option.value_exn maybe_disk |> (fun d -> Some { d with target = true }))

let go ls =
  let open List in
  let disks = drop ls 2 >>| parse_disk |> fold ~init:M.empty ~f:(fun acc d -> M.add ~key:(d.x, d.y) ~data:d acc) in
  let (x,y) = find_target disks in
  printf "Target data starts on disk (%d, %d)\n" x y;
  let disks' = set_target (x, y) disks in
  let first_situation = { moves = possible_moves disks'; disks = disks'} in
  let depth = walk 0 [first_situation] in
  printf "Moves: %d\n" depth


let () = In_channel.read_lines "input.txt" |> go
