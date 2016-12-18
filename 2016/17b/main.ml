open Core_kernel.Std
open List
open Printf

let md5 s = Digest.string s |> Digest.to_hex

type path = {
  position: int * int;
  rev_steps: char list;
  seed: string
}

let directions = ['U'; 'D'; 'L'; 'R']

let step path direction =
  let x, y = path.position in
  match direction with
  | 'U' -> (x, y - 1)
  | 'D' -> (x, y + 1)
  | 'L' -> (x - 1, y)
  | 'R' -> (x + 1, y)
  | _ -> failwith "ops!"

let is_room (x, y) = (x >= 1) && (x <=4) && (y >= 1) && (y <= 4)

let steps path =
  let hash = md5 @@ String.concat [path.seed; (path.rev_steps |> rev |> String.of_char_list)] in
  let is_open c = c >= 'b' && c <= 'f' in
  filter_mapi directions ~f:(fun i d ->
    let next_pos = step path d in
    if is_open hash.[i] && (next_pos |> is_room)
    then Some { path with position = next_pos; rev_steps = d :: path.rev_steps }
    else None)

let show_path path =
  let x, y = path.position in
  sprintf "path at:(%d,%d) steps:%s" x y (path.rev_steps |> rev |> String.of_char_list)

let initial_path = {
  position = (1,1);
  rev_steps = [];
  seed = "njfxhljp"
}

let walk_to x y =
  let are_we_there_yet path = let x', y' = path.position in (x = x') && (y = y') in
  let rec inner acc paths =
    let there_yet, not_there_yet = partition_tf paths ~f:are_we_there_yet in
    let acc' = append there_yet acc in
    match not_there_yet with
    | [] -> acc'
    | _ -> not_there_yet >>| steps |> concat |> inner acc' in
  inner [] [initial_path]

let () =
  walk_to 4 4 >>| (fun path -> length path.rev_steps) |> max_elt ~cmp:Pervasives.compare |> (fun ml -> printf "%d\n" (Option.value_exn ml))
