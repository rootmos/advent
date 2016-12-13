open Core_kernel.Std
open List
open Printf

let step key i = 
  let hash = sprintf "%s%d" key i |> Digest.string |> Digest.to_hex in
  if String.prefix hash 5 = "00000" then Sequence.Step.Yield (hash.[5], i + 1)
  else Sequence.Step.Skip (i + 1)

let go s =
  Sequence.unfold_step ~init:0 ~f:(step s)
    |> Fn.flip Sequence.take 8
    |> Sequence.to_list
    |> String.of_char_list
    |> printf "%s\n"

let () = go "reyedfim"
