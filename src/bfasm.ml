(*
 * This file is part of bfasm.
 * Copyright (C) 2014 Romain PORTE (MicroJoe).
 *
 * bfasm is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * bfasm is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with bfasm. If not, see <http://www.gnu.org/licenses/>.
 *)

(* An address for the interpreter, from 0 to sth like 30k *)
type adress = int

(* A value for the interpreter, which should be in 0 <= v <= 255 *)
type value = int

(* Available instructions, from high to low level *)
type instr =
  (* High level instructions *)
  | Add of adress * value
  | LocalAdd of value
  | Move of adress

  (* Final state low level instructions *)
  | Left
  | Right

(* Brainfuck code *)
type bf = string

(* Context-based brainfuck code with current pointer adress *)
type ctxbf = adress * string

(* Repeat a character n times *)
let repeat chr n =
  let s = String.create n in
  String.fill s 0 n chr;
  s

(* Reverse a string, like the Haskell builtin *)
let rec reverse is =
  let l = String.length is in
  let s = String.create l in
  for i = 1 to l - 1 do
    String.set s i (String.get is (l - i))
  done;
  s

(* Convert an instr to a ctxbf item *)
let rec bf_of_instr (addr:adress) previous inst =
  (* Function to call and return when nothing is left to be done ; can be
   * called in some specialized pattern matching entries in order to optimize
   * a little bit. *)
  let finalize = fun () -> (addr, String.concat "" previous) in
  match inst with
  (* Add *)
  | Add (dst, 0) -> finalize ()
  | Add (dst, value) ->
      let (new_addr, new_bf) = bf_of_instr addr previous (Move (dst)) in
      bf_of_instr new_addr (new_bf::previous) (LocalAdd (value))
  (* LocalAdd *)
  | LocalAdd 0 -> finalize ()
  | LocalAdd (value) ->
      let chr = if value > 0 then '+' else '-' in
      let s = repeat chr (abs value) in
      let b = String.concat "" (s::previous) in
      (* We have to reverse generated BF code because of lists which add to head
       * instead of to queue *)
      (addr, reverse b)
  (* Move *)
  | Move x when x == addr -> finalize ()
  | Move dst ->
      (* TODO:
       * handle recursivity in something like RecLeft and RecRight instrs *)
      let diff = addr - dst in
      let dir = if diff < 0 then Right else Left in
      let (new_addr, new_bf) = bf_of_instr addr previous dir in
      bf_of_instr new_addr (new_bf::previous) (Move dst)
  (* Left *)
  | Left ->
      (addr - 1, "<")
  (* Right *)
  | Right ->
      (addr + 1, ">")

(* Main program, tries to parse some high level instr *)
let () =
  (* Wow, this really looks like a monad, maybe do something about it
   * (implement a monad in OCaml ?) *)
  let (na, s) = bf_of_instr 0 [] (Add (10, -10)) in
  let (na', s') = bf_of_instr na [] (Move 0) in
  let (na2, s2) = bf_of_instr na' [] (Add (9, 10)) in
  Printf.printf "add 10 -10\n%s\nmove 0\n%s\nadd 9 10\n%s\n" s s' s2
