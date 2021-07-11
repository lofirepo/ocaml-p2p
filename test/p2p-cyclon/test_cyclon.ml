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
module Node = P2p.Node.Make (Node_id)
module View = P2p.View.Make (Node_id) (Node)
module Cyclon = P2p_cyclon.Make (Node_id) (Node) (View)

let u64 = Uint64.of_int64
let pf = Fmt.pf
let out = Fmt.stdout

let view_len = 7
let xchg_len = 3
let me = Node.init (u64 99L)

let my_view =
  View.add (Node.init (u64 1L))
    (View.add (Node.init (u64 2L))
       (View.add (Node.init (u64 3L))
          (View.add (Node.init (u64 4L))
             (View.add (Node.init (u64 5L))
                (View.add (Node.init (u64 6L))
                   (View.add (Node.init (u64 7L))
                      View.empty))))))

let my_recvd =
    View.add (Node.init (u64 10L))
      (View.add (Node.init (u64 20L))
         (View.add (Node.init (u64 3L))
            View.empty))

let test_gossip _ctx =
  printf "\nCYCLON GOSSIP\n";
  let view = my_view in
  let xview = View.empty in
  let (dst, sent, view) = Cyclon.initiate ~me ~view ~xview ~view_len ~xchg_len in
  let recvd = my_recvd in
  let dst =
    match dst with
    | Some dst -> dst
    | None -> assert_failure "No gossip target" in
  pf out "Gossip target: %a\n" Node.pp dst;
  pf out "Gossip sent (%d):\n%a\n" (View.length sent) View.pp sent;
  pf out "Gossip received (%d):\n%a\n" (View.length recvd) View.pp recvd;
  assert_equal (View.length sent) xchg_len;
  pf out "View before gossip (%d):\n%a\n" (View.length view) View.pp view;
  let view = Cyclon.merge ~me ~src:dst ~view ~xview ~view_len ~sent ~recvd ~xchg_len in
  pf out "View after gossip (%d):\n%a\n" (View.length view) View.pp view;
  assert_equal (View.length view) view_len;
  assert_equal (View.mem (Node.id me) view) false;
  let resp = Cyclon.respond ~view ~xview ~recvd ~view_len ~xchg_len ~src:me ~me in
  pf out "Gossip response:\n%a\n" View.pp resp;
  assert_equal (View.length resp) xchg_len

let suite =
  "suite">:::
    [
      "gossip">:: test_gossip;
    ]

let () =
  Nocrypto_entropy_unix.initialize ();
  run_test_tt_main suite
