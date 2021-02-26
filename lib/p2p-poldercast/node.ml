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

module Make
         (Node_id: P2p.S.NODE_ID)
       : S.NODE with type nid := Node_id.t = struct

  module Node = P2p.Node.Make (Node_id)

  type node = Node.t
  type t = {
      node : node;
      subs : Bitv.t;
    }

  (** Base *)

  let id t = Node.id t.node
  let age t = Node.age t.node
  let ver t = Node.ver t.node
  let zero_age t = { t with node = Node.zero_age t.node }
  let incr_age t = { t with node = Node.incr_age t.node }
  let set_age t age = { t with node = Node.set_age t.node age }
  let set_ver t ver = { t with node = Node.set_ver t.node ver }
  let incr_ver t = { t with node = Node.incr_ver t.node }

  let compare a b = Node.compare a.node b.node

  let distance a b = Node.distance a.node b.node

  let distance_ring a b = Node.distance_ring a.node b.node

  (** Extensions *)

  let init ?age ?ver id =
    let node = Node.init ?age ?ver id in
    let subs = Bitv.create 0 false in
    { node; subs }

  let subs t =
    t.subs

  let set_subs t subs =
    { t with subs }

  let sim a b =
    Blip.sim a.subs b.subs

  let to_string t =
    let buf = Buffer.create (Bitv.length t.subs) in
    Bitv.iter
      (fun b -> Buffer.add_char buf (char_of_int (if b then 1 else 0))) t.subs;
    Printf.sprintf "%s\n%s\n" (Node.to_string t.node) (Buffer.contents buf)

  let pp ppf t =
    Node.pp ppf t.node;
    Bitv.iter (fun b -> Fmt.pf ppf "%d" (if b then 1 else 0)) t.subs

end
