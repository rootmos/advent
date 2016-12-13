open Core_kernel.Std
open List
open Printf

let step key i =
  let hash = sprintf "%s%d" key i |> Digest.string |> Digest.to_hex in
  if String.prefix hash 5 = "00000" then
    let pos = hash.[5] in
    let dig = hash.[6] in
    if pos >= '0' && pos <= '7' then Sequence.Step.Yield ((Char.to_int pos - 48, dig), i + 1)
    else Sequence.Step.Skip (i + 1)
  else Sequence.Step.Skip (i + 1)

let go s =
  Sequence.unfold_step ~init:0 ~f:(step s)
    |> Sequence.delayed_fold
      ~init:Int.Map.empty
      ~f:(fun ds (p, d) ~k ->
          let ds' = if Int.Map.mem ds p then ds else Int.Map.add ds p d in
          if Int.Map.length ds' = 8 then ds' else k ds')
      ~finish:(fun _ -> failwith "end of the world, fire ze missiles!")
    |> Int.Map.to_alist |> sort ~cmp:(fun (a, _) (b, _) -> Pervasives.compare a b)
    >>| snd |> String.of_char_list
    |> printf "%s\n"

let () = go "reyedfim"
