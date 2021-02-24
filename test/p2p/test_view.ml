(*
  SPDX-FileCopyrightText: 2019 TG x Thoth
  SPDX-License-Identifier: AGPL-3.0-only

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
     (View.add (Node.init (u64 11L) ~age:3 ~ver:2)
        (View.add (Node.init (u64 13L))
           (View.add (Node.init (u64 17L))
              (View.add (Node.init (u64 23L))
                 (View.add (Node.init (u64 19L))
                    (View.add (Node.init (u64 49L))
                       (View.add (Node.init (u64 47L))
                          View.empty))))))))

let test_view _ctx =
  let v = View.incr_age my_view in
  assert_equal (View.length v) 8;
  let v = View.remove (Uint64.of_int64 17L) v in
  assert_equal (View.length v) 7;
  let n =
    match View.find (u64 11L) v with
    | Some n -> n
    | None -> assert_failure "11 not found" in
  assert_equal (Node.id n) (u64 11L);
  let n = Node.incr_age n in
  assert_equal (Node.age n) 5;
  assert_equal (Node.ver n) 2;
  let n = Node.zero_age n in
  assert_equal (Node.age n) 0;
  let n2 =
    match View.find (u64 13L) v with
    | Some n -> n
    | None -> assert_failure "13 not found" in
  assert_equal (Node.age n2) 1;
  assert_equal (Node.ver n2) 0;
  assert_equal (Node.compare n n2) (-1);
  assert_equal (Node.compare n n) 0;
  assert_equal (Node.compare n2 n) 1;
  let l = View.to_list v in
  assert_equal l [
      Node.init (u64 7L) ~age:1;
      Node.init (u64 11L) ~age:4 ~ver:2;
      Node.init (u64 13L) ~age:1;
      Node.init (u64 19L) ~age:1;
      Node.init (u64 23L) ~age:1;
      Node.init (u64 47L) ~age:1;
      Node.init (u64 49L) ~age:1;
    ];
  let v2 = View.of_list ((Node.init (u64 11L)) :: l) in
  assert_equal (View.to_list v2) l

let suite =
  "suite">:::
    [
      "view">:: test_view;
    ]

let () =
  Nocrypto_entropy_unix.initialize ();
  run_test_tt_main suite
