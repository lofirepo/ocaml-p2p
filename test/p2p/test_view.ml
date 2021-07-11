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
open Stdint

let u64 = Uint64.of_int64

module Node_id = P2p.Node_id
module Node = P2p.Node.Make (Node_id)
module View = P2p.View.Make (Node_id) (Node)

let my_view =
  (View.add (Node.init (u64 7L))
     (View.add (Node.init (u64 11L) ~age:3 ~version:2 ~trust:0.8)
        (View.add (Node.init (u64 13L) ~trust:0.4)
           (View.add (Node.init (u64 17L) ~trust:0.4)
              (View.add (Node.init (u64 23L))
                 (View.add (Node.init (u64 19L))
                    (View.add (Node.init (u64 49L))
                       (View.add (Node.init (u64 47L))
                          View.empty))))))))

let test_view _ctx =
  let my_view = View.inc_age my_view in
  assert_equal (View.length my_view) 8;
  let v = View.remove (Uint64.of_int64 19L) my_view in
  assert_equal (View.length v) 7;
  let n =
    match View.find (u64 11L) v with
    | Some n -> n
    | None -> assert_failure "11 not found" in
  assert_equal (Node.id n) (u64 11L);
  let n = Node.inc_age n in
  assert_equal (Node.age n) 5;
  assert_equal (Node.version n) 2;
  let n = Node.zero_age n in
  assert_equal (Node.age n) 0;
  let n2 =
    match View.find (u64 13L) v with
    | Some n -> n
    | None -> assert_failure "13 not found" in
  assert_equal (Node.age n2) 1;
  assert_equal (Node.version n2) 0;
  assert_equal (Node.compare n n2) (-1);
  assert_equal (Node.compare n n) 0;
  assert_equal (Node.compare n2 n) 1;
  Fmt.pf Fmt.stdout "View:\n%a\n" View.pp v;
  let l = View.to_list v in
  assert_equal l [
      Node.init (u64 7L) ~age:1;
      Node.init (u64 11L) ~age:4 ~version:2 ~trust:0.8;
      Node.init (u64 13L) ~age:1 ~trust:0.4;
      Node.init (u64 17L) ~age:1 ~trust:0.4;
      Node.init (u64 23L) ~age:1;
      Node.init (u64 47L) ~age:1;
      Node.init (u64 49L) ~age:1;
    ];
  let v2 = View.of_list ((Node.init (u64 11L)) :: l) in
  assert_equal (View.to_list v2) l;

  let rnode = match View.random my_view with
    | Some r -> r
    | None -> Node.init (u64 0L) in
  let rsamp = View.uniform_sample 4 my_view in
  let wsamp = View.weighted_sample 4 my_view in
  let fview = View.adjust_trust 0.5 my_view in
  let fview = View.filter_trust 0.1 fview in
  let min = View.min_trust my_view in
  let max = View.max_trust my_view in

  Fmt.pf Fmt.stdout "Random:\n%a\n" Node.pp rnode;
  Fmt.pf Fmt.stdout "Random sample:\n%a\n" View.pp rsamp;
  Fmt.pf Fmt.stdout "Weighted sample:\n%a\n" View.pp wsamp;
  Fmt.pf Fmt.stdout "Adjust trust by 0.5 & filter out entries below 0.1 trust\n%a\n" View.pp fview;
  Fmt.pf Fmt.stdout "Trust: %.2f..%.2f\n" min max

let suite =
  "suite">:::
    [
      "view">:: test_view;
    ]

let () =
  Nocrypto_entropy_unix.initialize ();
  run_test_tt_main suite
