open Core_kernel.Std
open List

let go l =
  let raw_dirs = String.split ~on:',' l |> map ~f:String.strip in
  iter ~f:print_string raw_dirs

let () = 
  In_channel.with_file "input.txt" ~f:(fun f -> In_channel.input_all f |> go)
