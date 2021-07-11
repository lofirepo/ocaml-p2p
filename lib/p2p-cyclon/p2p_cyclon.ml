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

(**

{1 CYCLON: Inexpensive Membership Management for Unstructured P2P Overlays}

This is an OCaml implementation of the CYCLON protocol as specified in the paper
{{:https://www.distributed-systems.net/my-data/papers/2005.jnsm.pdf}
CYCLON: Inexpensive Membership Managementfor Unstructured P2P Overlays}.

{2 Protocol}

+ Increase by one the age of all neighbors.
+ Select neighbor {e Q} with the highest age among all neighbors, and {e l - 1} other random neighbors.
+ Replace {e Q}'s entry with a new entry of age 0 and with {e P}'s address.
+ Send the updated subset to peer {e Q}.
+ Receive from {e Q} a subset of no more that {e i} of its own entries.
+ Discard entries pointing at {e P} and entries already contained in {e P}'s cache.
+ Update {e P}'s cache to include {e all} remaining entries, by {e firstly} using empty cache slots (if any),
  and {e secondly} replacing entries among the ones sent to {e Q}.

{e Quoted from the paper above.}

{2 Security}

CYCLON is not secure against malicious nodes deviating from the protocol.
See {{:https://github.com/p2pcollab/ocaml-urps} URPS} for a stream sampler
implementation that achieves uniformity in presence of malicious nodes
and which can be used together with CYCLON.
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
