open Core_kernel.Std
open List
open Printf

let counts xs = sort ~cmp:Pervasives.compare xs |> group ~break:(<>)
    >>| (fun ys -> (hd_exn ys, length ys))

let go ls =
  map ~f:String.to_list ls |> transpose_exn
  >>| (fun xs -> counts xs
        |> sort ~cmp:(fun (_, a) (_, b) -> Pervasives.compare a b)
        |> hd_exn |> fst)
  |> String.of_char_list |> printf "%s\n"

let () = In_channel.read_lines "input.txt" |> go
