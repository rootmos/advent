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

let no_steps path = length path - 1

let walk_until n =
  let steps visited (x, y) =
    let ps = [(x-1,y); (x+1,y); (x,y-1); (x,y+1)] in
    filter ps ~f:(fun p -> not (S.mem visited p) && is_open p) in

  let rec inner visited paths =
    let visited' = fold paths ~init:visited ~f:(fun vs path -> S.add vs (hd_exn path)) in
    match filter paths ~f:(fun path -> (length path) - 1 < n) with
    | [] -> visited'
    | paths' ->
        let paths'' = map paths' ~f:(fun path ->
          steps visited' (hd_exn path) >>| (fun p -> p :: path)) in
        inner visited' (concat paths'') in

  inner S.empty [[(1,1)]]

let () =
  let visited = walk_until 50 in
  visited |> S.length |> printf "%d\n"
