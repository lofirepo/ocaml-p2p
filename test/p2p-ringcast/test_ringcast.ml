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
module Msg_id = P2p.Msg_id
module Ringcast =
  P2p_ringcast.Make (Node_id) (Node) (View) (Msg_id)

let u64 = Uint64.of_int64
let pf = Fmt.pf
let out = Fmt.stdout

let view_len = 8
let xchg_len = 4
let me = Node.init (u64 16L)

let my_view =
  View.add (Node.init (u64 7L))
    (View.add (Node.init (u64 11L))
       (View.add (Node.init (u64 13L))
          (View.add (Node.init (u64 17L))
             (View.add (Node.init (u64 19L))
                (View.add (Node.init (u64 23L))
                   (View.add (Node.init (u64 41L))
                      (View.add (Node.init (u64 49L))
                         View.empty)))))))

let my_view_str =
  View.add (Node.init (u64 5L))
    (View.add (Node.init (u64 10L))
       (View.add (Node.init (u64 20L))
          (View.add (Node.init (u64 30L))
             (View.add (Node.init (u64 40L))
                (View.add (Node.init (u64 45L))
                   (View.add (Node.init (u64 50L))
                      (View.add (Node.init (u64 0L))
                         View.empty)))))))

let my_recvd =
    View.add (Node.init (u64 5L))
      (View.add (Node.init (u64 10L))
         (View.add (Node.init (u64 20L))
            (View.add (Node.init (u64 30L))
                  View.empty)))

let test_gossip _ctx =
  printf "\nRINGCAST GOSSIP\n";
  let view = my_view in
  let xview = my_view_str in
  let (dst, sent, view) = Ringcast.initiate ~me ~view ~xview ~view_len ~xchg_len in
  let recvd = my_recvd in
  let dst =
    match dst with
    | Some dst -> dst
    | None -> assert_failure "No gossip target" in
  pf out "Gossip target: %a\n" Node.pp dst;
  pf out "Gossip sent (%d):\n%a\n" (View.length sent) View.pp sent;
  assert_equal (View.length sent) xchg_len;
  pf out "Gossip received (%d):\n%a\n" (View.length recvd) View.pp recvd;
  pf out "View before gossip (%d):\n%a\n" (View.length view) View.pp view;
  let view = Ringcast.merge ~me ~src:dst ~view ~xview ~sent ~recvd ~view_len ~xchg_len in
  pf out "View after gossip (%d):\n%a\n" (View.length view) View.pp view;
  assert_equal (View.length view) view_len;
  assert_equal (View.mem (Node.id me) view) false;
  let resp = Ringcast.respond ~view ~xview ~recvd ~view_len ~xchg_len ~src:me ~me in
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
