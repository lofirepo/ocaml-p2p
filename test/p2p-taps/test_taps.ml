(*
  Copyright (C) 2021 TG x Thoth

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

module Taps_config : P2p_taps.S.CONFIG = struct
  let min_trust = 0.1
  let trust_transitivity = 0.9
end

module Node_id = P2p.Node_id
module Node = P2p.Node.Make (Node_id)
module View = P2p.View.Make (Node_id) (Node)
module Taps = P2p_taps.Make (Node_id) (Node) (View) (Taps_config)

let u64 = Uint64.of_int64
let pf = Fmt.pf
let out = Fmt.stdout

let view_len = 5
let xchg_len = 3
let me = Node.init (u64 99L) ~trust:1.0 ~known:true

let my_view =
  View.add (Node.init (u64 1L) ~trust:0.5 ~known:true)
    (View.add (Node.init (u64 2L) ~trust:0.9 ~known:true)
       (View.add (Node.init (u64 3L) ~trust:0.3 ~known:true)
          (View.add (Node.init (u64 4L) ~trust:0.5 ~known:true)
             (View.add (Node.init (u64 5L) ~trust:0.7 ~known:true)
                (View.add (Node.init (u64 6L) ~trust:0.9 ~known:true)
                   (View.add (Node.init (u64 7L) ~trust:0.2 ~known:true)
                      View.empty))))))

let my_recvd =
    View.add (Node.init (u64 10L) ~trust:0.8)
      (View.add (Node.init (u64 20L) ~trust:0.6)
         (View.add (Node.init (u64 3L) ~trust:0.4)
            View.empty))

let test_gossip _ctx =
  printf "\nTAPS GOSSIP\n\n";
  let view = View.empty in
  let xview = my_view in
  let (dst, sent, view) = Taps.initiate ~me ~view ~xview ~view_len ~xchg_len in
  let recvd = my_recvd in
  let dst =
    match dst with
    | Some dst -> dst
    | None -> assert_failure "No gossip target" in
  pf out "Me: %a" Node.pp me;
  pf out "Gossip target: %a\n" Node.pp dst;
  pf out "Gossip sent (len %d):\n%a\n" (View.length sent) View.pp sent;
  pf out "Gossip received (len %d):\n%a\n" (View.length recvd) View.pp recvd;
  assert_equal (View.length sent) xchg_len;
  pf out "View before gossip (len %d):\n%a\n" (View.length view) View.pp view;
  let view = Taps.merge ~me ~src:dst ~view ~xview ~view_len ~sent ~recvd ~xchg_len in
  pf out "View after gossip (len %d):\n%a\n" (View.length view) View.pp view;
  assert_equal (View.length view) view_len;
  assert_equal (View.mem (Node.id me) view) false;
  let resp = Taps.respond ~view ~xview ~recvd ~view_len ~xchg_len ~src:dst ~me in
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
