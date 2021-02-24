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

(**
{1 VICINITY: P2P topology management protocol}

This is an implementation of VICINITY, a P2P topology management protocol
described in the paper {{:https://hal.inria.fr/hal-01480790/document}
VICINITY: A Pinch of Randomness Brings out the Structure}.
The protocol takes care of overlay construction & maintenance,
and can be used for e.g. clustering nodes in groups
or organizing them in a coordinate system.
*)

(** Functor building an implementation of Vicinity
    given a [Node_id], [Node], and gossip [View]. *)
module Make
         (Node_id : P2p.S.NODE_ID)
         (Node : S.NODE with type nid := Node_id.t)
         (View : P2p.S.VIEW with type nid := Node_id.t
                             and type node := Node.t)
       : P2p.S.GOSSIP with type node := Node.t
                       and type view := View.t = struct

  module Rng = Nocrypto.Rng

  type s =
    {
      sim: float; (** similarity measure *)
      node: Node.t;
    }

  (** select [xchg_len] nodes closest to [dst] from [view] *)
  let closest ~view ~dst ~xchg_len =
    let dlist =
      List.stable_sort
        (fun a b ->
          if a.sim = b.sim then 0
          else if a.sim < b.sim then -1
          else 1)
        (List.sort
           (fun _a _b -> if Rng.Int.gen 2 = 0 then 1 else -1)
           (View.fold
              (fun _nid node lst ->
                { node; sim = Node.sim dst node } :: lst)
              view [])) in
    List.fold_left
      (fun dview dnode ->
        if View.length dview < xchg_len
        then View.add dnode.node dview
        else dview)
      View.empty
      dlist

  let initiate ~view ~xview ~me ~xchg_len  =
    let dst = View.oldest view in
    match dst with
    | Some dst ->
       let view = View.remove (Node.id dst) view in
       let view = View.incr_age view in
       let uview = View.union view xview in
       let xchg_len = xchg_len - 1 in
       let xchg = closest ~dst:me ~xchg_len ~view:uview in
       let xchg = View.add me xchg in
       (Some dst, xchg, view)
    | None ->
       (None, View.empty, view)

  let respond ~view ~xview ~recvd ~src ~me ~xchg_len =
    let uview = View.add me view in
    let uview = View.union uview xview in
    let uview = View.filter (* remove recvd nodes *)
                  (fun nid _node -> not (View.mem nid recvd))
                  uview in
    closest ~dst:src ~xchg_len ~view:uview

  let merge ~view ~view_len ~sent ~recvd ~xchg_len ~me =
    let _sent = sent in
    let recvd = View.remove (Node.id me) recvd in
    let recvd = View.random_subset xchg_len recvd in
    let recvd = View.zero_age recvd in
    let uview = View.union view recvd in
    closest ~dst:me ~xchg_len:view_len ~view:uview
end

(** Signatures *)
module S = S
