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

open Ringcast
open OUnit2
open Printf

let view_len = 8
let xchg_len = 4
let my_nid = "ME"
let my_data = 43

let print_view msg view =
  printf "\n%s\n" msg;
  View.iter (fun id n -> Printf.printf "%s: %d (%d)\n" id n.data n.age) view;;

let print_xchg msg xchg =
  Printf.printf "\n%s\n" msg;
  View.iter (fun id n -> Printf.printf "%s: %d\n" id n.data) xchg;;

let opt2str v =
  match v with
  | Some v -> v
  | None -> "-"

let opt2int v =
  match v with
  | Some v -> v
  | None -> -1

let my_view =
  add "a" 7
    (add "b" 11
       (add "c" 13
          (add "d" 17
             (add "e" 19
                (add "f" 23
                   (add "g" 41
                      (add "h" 49
                         View.empty)))))))

let my_recvd =
  (add "W" 10
     (add "X" 20
        (add "Y" 30
           (add "Z" 40
              View.empty))))

let my_view_str =
  add "A" 5
    (add "B" 10
       (add "C" 20
          (add "D" 30
             (add "E" 40
                (add "F" 45
                   (add "G" 50
                      (add "H" 0
                         View.empty)))))))

let distance _nid1 data1 _nid2 data2 =
  let min_nid = 0 in
  let max_nid = 50 in
  let nid1 = data1 in
  let nid2 = data2 in
  let d = abs (nid2 - nid1) in
  let d = if d <= (max_nid - min_nid) / 2
          then d
          else max_nid + 1 - d in
  let d = if (nid1 - nid2 = d) || (nid1 < nid2 && nid2 - nid1 != d)
          then -1 * d
          else d in
  (* Printf.printf "distance %d %d = %d\n" nid1 nid2 d; *)
  d

let test_add _ctx =
  let view = my_view in
  print_view "add" view;
  assert_equal (View.cardinal view) view_len

let test_xchg _ctx =
  let view = my_view in
  let (nid, data, sent, _view) =
    make_exchange view my_view_str xchg_len my_nid my_data distance in
  printf "\nSEND TO %s (%d)\n" (opt2str nid) (opt2int data);
  print_xchg "SEND:" sent;
  print_newline ();
  assert_equal (View.cardinal sent) xchg_len

let test_recv _ctx =
  let view = my_view in
  let (nid, data, sent, view) =
    make_exchange view my_view_str xchg_len my_nid my_data distance in
  let recvd = my_recvd in
  printf "\n\nSEND TO %s (%d)\n" (opt2str nid) (opt2int data);
  print_view "VIEW BEFORE:" view;
  print_xchg "SENT:" sent;
  print_xchg "RECVD:" recvd;
  assert_equal (View.cardinal view) (view_len - 1);
  assert_equal (View.cardinal sent) xchg_len;
  let view2 = merge_recvd view view_len recvd xchg_len my_nid my_data distance in
  print_view "VIEW AFTER:" view2;
  assert_equal (View.cardinal view2) view_len;
  assert_equal (View.mem "ME" view2) false;
  let (rnid, rdata) = ("x", 5) in
  let resp =
    make_response view2 my_view_str xchg_len rnid rdata recvd
      my_nid my_data distance in
  printf "\nRESPOND TO %s (%d)\n" rnid rdata;
  print_xchg "RESP:" resp;
  assert_equal (View.cardinal resp) xchg_len;
  let (rnid, _rdata) = View.choose recvd in
  assert_equal (View.mem rnid resp) false

let suite =
  "suite">:::
    [
      "add">:: test_add;
      "exchange">:: test_xchg;
      "receive">:: test_recv;
    ]

let () =
  Random.self_init ();
  run_test_tt_main suite
