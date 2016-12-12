open Core_kernel.Std

let go = print_string l

let () = 
  In_channel.with_file "input.txt" ~f:(fun f -> In_channel.input_all f |> go)
