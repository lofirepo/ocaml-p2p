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

(** Functor building an implementation of Cyclon
    given a [Node_id], [Node], and gossip [View]. *)
module Make
         (Node_id : P2p.S.NODE_ID)
         (Node : P2p.S.NODE with type nid := Node_id.t)
         (View : P2p.S.VIEW with type nid := Node_id.t
                             and type node := Node.t)
       : P2p.S.GOSSIP with type node := Node.t
                       and type view := View.t = struct

  (** Initiate gossip exchange **)
  let initiate ~view ~xview ~me ~view_len ~xchg_len =
    let _view_len = view_len in
    let dst = View.oldest view in
    match dst with
    | Some dst ->
       let view = View.remove (Node.id dst) view in
       let view = View.inc_age view in
       let uview = View.union view xview in
       let xchg = View.uniform_sample (xchg_len - 1) uview in
       let xchg = View.add me xchg in
       (Some dst, xchg, view)
    | None -> (* view empty *)
       (None, View.empty, view)

  (** Respond to gossip exchange *)
  let respond ~view ~xview ~recvd ~src ~me ~view_len ~xchg_len =
    let _recvd = recvd and _src = src and _me = me and _view_len = view_len in
    let uview = View.union view xview in
    View.uniform_sample xchg_len uview

  (** Merge received view into current view *)
  let merge ~view ~xview ~sent ~recvd ~src ~me ~view_len ~xchg_len =
    let _xview = xview and _src = src in
    let sent = View.remove (Node.id me) sent in
    let recvd = View.remove (Node.id me) recvd in
    let recvd = View.uniform_sample xchg_len recvd in
    let recvd = View.zero_age recvd in
    let rec merge view sent recvd =
      if 0 < View.length recvd then
        match View.random recvd with
        | (Some rnode) ->
           if View.length view < view_len then
             (* fill an empty slot in view *)
             let view = View.add rnode view in
             let recvd = View.remove (Node.id rnode) recvd in
             merge view sent recvd
           else (* replace a sent entry in view with a received one *)
             (match View.random sent with
              | Some snode ->
                 let view = View.add rnode view in
                 let view =
                   if view_len < View.length view
                   then View.remove (Node.id snode) view
                   else view in
                 let sent = View.remove (Node.id snode) sent in
                 let recvd = View.remove (Node.id rnode) recvd in
                 merge view sent recvd
              | _ -> view)
        | _ -> view
      else view in
    merge view sent recvd

end
