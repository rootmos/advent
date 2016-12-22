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

let avail d = d.size - d.used

let is_viable a b =
  (a.x <> b.x || a.y <> b.y) &&
  a.used > 0 && a.used <= avail b

let go ls =
  let open List in
  let disks = drop ls 2 >>| parse_disk in
  let no_viable = fold disks ~init:0 ~f:(fun s a ->
    fold disks ~init:s ~f:(fun s' b ->
      if is_viable a b then s' + 1 else s')) in
  printf "viable pairs: %d\n" no_viable

let () = In_channel.read_lines "input.txt" |> go
