open Core_kernel.Std
open Printf
open Re2

type disk = {
  x: int; y: int;
  size: int;
  used: int
}

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
      used = get_int ms.(4)})
    |> ok_exn)

let go ls =
  let open List in
  let disks = drop ls 2 >>| parse_disk in
  disks >>| show_disk >>| printf "%s\n" |> ignore

let () = In_channel.read_lines "input.txt" |> go
