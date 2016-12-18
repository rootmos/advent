open Core_kernel.Std
open List
open Printf


let dragon cs =
  let flip_rev cs =
    let rec inner acc = function
      | '1' :: tail -> inner ('0' :: acc) tail
      | '0' :: tail -> inner ('1' :: acc) tail
      | _ :: _ -> failwith "ops!"
      | [] -> acc in
    inner [] cs in
  append cs ('0' :: flip_rev cs)

let dragon_stream initial = Sequence.unfold ~init:initial ~f:(fun cs ->
  let cs' = dragon cs in Some (cs', cs'))

let rec checksum cs =
  if (length cs) land 1 = 1 then cs
  else
    let pairs = groupi ~break:(fun i _ _ -> (i land 1) = 0) cs in
    let comparator = function
      | c :: c' :: [] when c = c' -> '1'
      | c :: c' -> '0'
      | _ -> failwith "uneven list!" in
    pairs >>| comparator |> checksum

let () =
  let initial = String.to_list "11011110011011101" in
  let l = 272 in
  let data = dragon_stream initial
      |> Sequence.drop_while ~f:(fun cs -> length cs < l)
      |> Sequence.hd_exn |> (fun cs -> take cs l) in
  let csum = checksum data in
  printf "%s %s\n" (String.of_char_list csum) (String.of_char_list data)
