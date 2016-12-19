open Core_kernel.Std
open List
open Printf

let n = 3004953

type elf = {
  id: int;
  presents: int;
  next: int
}

let elves = Array.init n ~f:(fun i -> { id = i; presents = 1; next = (i + 1) % n})

let show_elf elf =
  sprintf "elf:%d, presents:%d, next:%d" (elf.id + 1) elf.presents elf.next

let show_elves () =
  Array.iter elves ~f:(fun elf -> printf "(%s) " (show_elf elf));
  printf "\n"

let is_greedy elf = elf.next = elf.id

let rec progress elf =
  if is_greedy elf then elf
  else
    let neighbor = elves.(elf.next) in
    let elf' = { elf with presents = elf.presents + neighbor.presents; next = neighbor.next } in
    elves.(elf.id) <- elf';
    progress elves.(elf'.next)

let () =
  let greedy_elf = progress elves.(0) in
  printf "The greedy elf: %d\n" (greedy_elf.id + 1)

