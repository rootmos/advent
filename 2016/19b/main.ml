open Core_kernel.Std
open List
open Printf

let n = 3004953

type elf = {
  id: int;
  prev: int;
  next: int
}

let elves = Array.init n ~f:(fun i -> { id = i; prev = (i - 1) % n; next = (i + 1) % n})

let show_elf elf =
  sprintf "elf:%d, next:%d" (elf.id + 1) elf.next

let show_elves () =
  Array.iter elves ~f:(fun elf -> printf "(%s) " (show_elf elf));
  printf "\n"

let is_greedy elf = elf.next = elf.id

let remove_elf elf =
  let prev = elves.(elf.prev) in
  elves.(prev.id) <- { prev with next = elf.next };

  let next = elves.(elf.next) in
  elves.(next.id) <- { next with prev = elf.prev }

let rec neighbor n e = if n = 0 then e else neighbor (n - 1) elves.(e.next)

let rec progress no_elves op elf =
  if is_greedy elf then elf
  else
    let op_steps = 1 + (no_elves land 1) in
    let op' = neighbor op_steps op in
    (*printf "on elf %d, removing elf %d, next op %d\n" (elf.id + 1) (op.id + 1) (op'.id + 1);*)
    remove_elf op;
    progress (no_elves - 1) elves.(op'.id) elves.(elf.next)

let () =
  let start = elves.(0) in
  let op = neighbor (n lsr 1) start in

  let greedy_elf = progress n op elves.(0) in
  printf "The greedy elf: %d\n" (greedy_elf.id + 1)

