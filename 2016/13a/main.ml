open Core_kernel.Std
open List
open Printf

module S = Set.Make(struct include Tuple.Make(Int)(Int) include Tuple.Comparable(Int)(Int) end)

let favorite_number = 1350

let bit_count n =
  let rec inner acc m = if m = 0 then acc else inner (acc + (m land 1)) (m lsr 1) in
  inner 0 n

let is_even n = (n land 1) = 0

let is_open (x, y) =
  if (x < 0) || (y < 0) then false
  else x*x + 3*x + 2*x*y + y + y*y + favorite_number |> bit_count |> is_even

let print_maze w h path =
  iter (range 0 h) ~f:(fun y ->
    iter (range 0 w) ~f:(fun x ->
      if mem path (x, y) then printf "O"
      else if is_open (x, y) then printf "." else printf "#");
    printf "\n"
  )

let walk_to x y =
  let are_we_there_yet path = hd_exn path = (x, y) in

  let steps visited (x, y) =
    let ps = [(x-1,y); (x+1,y); (x,y-1); (x,y+1)] in
    filter ps ~f:(fun p -> not (S.mem visited p) && is_open p) in

  let rec inner visited paths =
    match find paths are_we_there_yet with
    | Some p -> p
    | None ->
        let visited' = fold paths ~init:visited ~f:(fun vs path -> S.add vs (hd_exn path)) in
        let paths' = map paths ~f:(fun path ->
          steps visited' (hd_exn path) >>| (fun p -> p :: path)) in
        inner visited' (concat paths') in

  inner S.empty [[(1,1)]]

let steps path = length path - 1

let () =
  let path = walk_to 31 39 in
  print_maze 45 45 path;
  path |> steps |> printf "%d\n"
