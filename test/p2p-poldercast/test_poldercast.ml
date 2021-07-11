(*
  Copyright (C) 2019 TG x Thoth

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU Affero General Public License as published by
  the Free Software Foundation, either version 3 of the License.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Affero General Public License for more details.

  You should have received a copy of the GNU Affero General Public License
  along with this program.  If not, see <https://www.gnu.org/licenses/>.
*)

open OUnit2
open Printf
open Stdint

module Node_id = P2p.Node_id
module Group_id = P2p.Group_id
module Msg_id = P2p.Msg_id

module Node = P2p.Node.Make (Node_id)
module Group = P2p.Group.Make (Group_id)
module View = P2p.View.Make (Node_id) (Node)

module Ringcast = P2p_ringcast.Make (Node_id) (Node) (View) (Msg_id)

module Poldercast = P2p_poldercast.Make (Node_id) (Group_id) (Node) (Group) (View) (Msg_id) (Ringcast)
module Pub = Poldercast.Pub
module Sub = Poldercast.Sub

let u64 = Uint64.of_int64
let pf = Fmt.pf
let out = Fmt.stdout
let e = 10.

let test_sub _ctx =
  printf "\nPOLDERCAST SUB\n";
  let sub = Sub.add (Group.init (u64 11L)) ()
              (Sub.add (Group.init (u64 23L)) ()
                 (Sub.add (Group.init (u64 37L)) ()
                    (Sub.add (Group.init (u64 41L)) ()
                       (Sub.empty)))) in
  let n23 = Node.init (u64 23L) in
  let _bits = Sub.bloom sub in
  let blip = Sub.blip sub e in
  let n23 = Node.set_subs n23 blip in
  printf "n23:\n%s" (Node.to_string n23);

  let n42 = Node.init (u64 42L) in
  let _bits = Sub.bloom sub in
  let blip = Sub.blip sub e in
  let n42 = Node.set_subs n42 blip in
  printf "n42:\n%s" (Node.to_string n42);

  let sim = Blip.sim (Node.subs n23) (Node.subs n23) in
  assert_equal sim 1.;
  let sim = Blip.sim (Node.subs n23) (Node.subs n42) in
  printf "sim n23 n42 = %.2f\n" sim;

  let g41 = (u64 41L) in
  assert_equal (Sub.mem g41 sub) true;
  assert_equal (Sub.length sub) 4;

  let sub = Sub.remove g41 sub in
  assert_equal (Sub.mem g41 sub) false;
  assert_equal (Sub.length sub) 3

let suite =
  "suite">:::
    [
      "sub">:: test_sub;
    ]

let () =
  Nocrypto_entropy_unix.initialize ();
  run_test_tt_main suite
