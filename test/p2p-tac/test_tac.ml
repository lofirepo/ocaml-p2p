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

module Tac_config : P2p_tac.S.CONFIG = struct
  let trust_weight = 1.
  let min_trust = 0.1
end

module Node_id = P2p.Node_id
module Node = P2p.Node.Make (Node_id)
module View = P2p.View.Make (Node_id) (Node)
module Tac = P2p_tac.Make (Node_id) (Node) (View) (Tac_config)

let u64 = Uint64.of_int64
let pf = Fmt.pf
let out = Fmt.stdout

let view_len = 7
let xchg_len = 5
let do_blip = false

let blip bf =
  let bits = Bloomf.bits bf in
  let e = 13. in
  let _m, k = Bloomf.params bf in
  let p = Blip.p e k in
  Blip.flip bits p

let subs subsl =
  let bf = Bloomf.create 10 in
  List.iter (fun sub -> Bloomf.add bf sub) subsl;
  if do_blip
  then blip bf
  else Bloomf.bits bf

let test_gossip _ctx =
  printf "\nTAC GOSSIP\n\n";

  let me = Node.init (u64 23L) ~trust:1.0 ~known:true ~sim:1.0  ~subs:(subs ["A"; "B"; "C"]) in

  let my_xview =
    View.add (Node.init (u64 7L) ~trust:0.9 ~subs:(subs ["A"]))
      (View.add (Node.init (u64 11L) ~trust:0.8 ~subs:(subs ["A"; "B"]))
         (View.add (Node.init (u64 13L) ~trust:0.7 ~subs:(subs ["D"]))
            (View.add (Node.init (u64 17L) ~trust:0.6 ~subs:(subs ["B"]))
               (View.add (Node.init (u64 19L) ~trust:0.5 ~subs:(subs ["C"]))
                  (View.add (Node.init (u64 29L) ~trust:0.4 ~subs:(subs ["B"; "C"]))
                     (View.add (Node.init (u64 37L) ~trust:0.3 ~subs:(subs ["A"; "B"; "C"]))
                        View.empty)))))) in

  let my_view =
    View.add (Node.init (u64 10L) ~trust:0.9 ~subs:(subs ["A"]))
      (View.add (Node.init (u64 20L) ~trust:0.8 ~subs:(subs ["A"; "B"]))
         (View.add (Node.init (u64 30L) ~trust:0.7 ~subs:(subs ["D"]))
            (View.add (Node.init (u64 40L) ~trust:0.6 ~subs:(subs ["B"; "D"]))
               (View.add (Node.init (u64 50L) ~trust:0.5 ~subs:(subs ["C"]))
                  (View.add (Node.init (u64 60L) ~trust:0.4 ~subs:(subs ["B"; "C"]))
                     (View.add (Node.init (u64 70L) ~trust:0.3 ~subs:(subs ["A"; "B"; "C"]))
                        View.empty)))))) in

  let my_recvd =
    View.add (Node.init (u64 60L) ~trust:0.6 ~subs:(subs ["A"; "B"; "C"]))
      (View.add (Node.init (u64 70L) ~trust:0.7 ~subs:(subs ["A"; "B"]))
         (View.add (Node.init (u64 80L) ~trust:0.8 ~subs:(subs ["B"; "C"]))
            (View.add (Node.init (u64 90L) ~trust:0.9 ~subs:(subs ["C"]))
               (View.add (Node.init (u64 100L) ~trust:1.0 ~subs:(subs ["A"; "B"; "C"]))
                  View.empty)))) in

  let view = my_view in
  let xview = my_xview in
  let (dst, sent, view) = Tac.initiate ~me ~view ~xview ~view_len ~xchg_len in
  let recvd = my_recvd in
  let dst =
    match dst with
    | Some dst -> dst
    | None -> assert_failure "No gossip target" in
  pf out "Me: %a" Node.pp me;
  pf out "Gossip target: %a\n" Node.pp dst;
  pf out "Initial TAPS view (len %d):\n%a\n" (View.length view) View.pp xview;
  pf out "Initial TAC view (len %d):\n%a" (View.length view) View.pp view;
  pf out "Trust sum of TAC view: %f\n\n" (View.sum_trust view);
  pf out "Gossip sent to target (len %d):\n%a\n" (View.length sent) View.pp sent;
  assert_equal (View.length sent) xchg_len;
  pf out "Gossip received from target (len %d):\n%a\n" (View.length recvd) View.pp recvd;
  let view = Tac.merge ~me ~src:dst ~view ~xview ~sent ~recvd ~view_len ~xchg_len in
  pf out "Merged TAC view after gossip (len %d):\n%a" (View.length view) View.pp view;
  assert_equal (View.length view) view_len;
  assert_equal (View.mem (Node.id me) view) false;
  pf out "Trust sum of TAC view: %f\n\n" (View.sum_trust view);
  let resp = Tac.respond ~view ~xview ~recvd ~view_len ~xchg_len ~src:dst ~me in
  pf out "Gossip response to target:\n%a\n" View.pp resp;
  assert_equal (View.length resp) xchg_len

let suite =
  "suite">:::
    [
      "gossip">:: test_gossip;
    ]

let () =
  Nocrypto_entropy_unix.initialize ();
  run_test_tt_main suite
