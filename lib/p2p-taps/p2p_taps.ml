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

(**

{1 TAPS: Trust-Aware Peer Sampling}

This is an OCaml implementation of a Trust-Aware Peer Sampling protocol
based on the TAPS2 protocol described in the thesis
{{:https://tel.archives-ouvertes.fr/tel-01135867/file/JEGOU_Arnaud.pdf}
Harnessing the power of implicit and explicit socialnetworks through decentralization}.

{2 Protocol}

The protocol is based on a gossip exchange,
similar to random peer sampling protocols such as CYCLON,
but biases node selection based on local trust information.

This implementation differs from TAPS2 in two key ways:
 + trust values are asymmetric,
   thus no agreement necessary between nodes about symmetric trust values
 + nodes only use local information to establish trust values,
   without relying on multi-hop trust paths.
   They store trust information about direct friends,
   and discover friends-of-friends via a private set intersection protocol;
   future work remains to research asynchronous trust that considers multi-hop trust as well.

Each node maintains two views:
 + an EXPLICIT view of nodes with associated local asymmetric trust values,
 + a dynamic TAPS view that view exchanges can modify

The view exchange mechanism this module implements is identical to the one from TAPS2,
with the following properties:
 + Biased view selection: prefer more trusted entries when merging received views
 + Continuous bootsrapping: continuously exchange information between both views of both types of nodes,
  thus TAPS-TAPS, TAPS-EXPL, EXPL-TAPS, EXPL-EXPL exchanges all happen in a randomized manner
 *)

(** Default TAC protocol configuration parameters *)
module Default_config : S.CONFIG = struct
  let min_trust = 0.1
  let trust_transitivity = 0.9
end

(** Functor building an implementation of TAPS
    given a [Node_id], [Node], and gossip [View]. *)
module Make
         (Node_id : P2p.S.NODE_ID)
         (Node : P2p.S.NODE with type nid := Node_id.t)
         (View : P2p.S.VIEW with type nid := Node_id.t
                             and type node := Node.t)
         (Config : S.CONFIG)
       : P2p.S.GOSSIP with type node := Node.t
                       and type view := View.t = struct

  (** Generate entries to send to [dst] during an exchange.

      Take a uniform random sample of size [xchg_len]
      from the union of [view] and [xview] *)
  let gen_xchg view xview dst me xchg_len =
    (* choose entries to send from both views *)
    let uview = View.union view xview in
    let uview = View.remove (Node.id dst) uview in
    let xchg = View.uniform_sample (xchg_len - 1) uview in
    View.add me xchg

  (** Initiate gossip exchange.

      Return selected destination [dst] node to exchange with, or None if both [xview] and [view] is empty,
      the [xchg] entries to send, and the updated TAPS [view] with increased age

      @param view      TAPS view
      @param xview     explicit view that contains nodes with locally assigned trust values
      @param me        this node
      @param view_len  max length of TAPS view
      @param xchg_len  max length of gossip exchange

      @return (dst option, xchg, view)
   *)
  let initiate ~view ~xview ~me ~view_len ~xchg_len =
    (* if view is empty, initialize it with random elements from explicit view *)
    let view =
      if View.is_empty view
      then View.uniform_sample view_len xview
      else view in
    (* choose either the implicit or explicit view at random
       to pick the destination from *)
    let rview = View.choose_view view xview in
    (* pick the oldest entry from the chosen view as the gossip target (destination),
       (when the explicit view is chosen, a random node is picked) *)
    let dst = View.oldest rview in
    match dst with
    | Some dst ->
       let xchg = gen_xchg view xview dst me xchg_len in
       let view = View.inc_age view in
       (Some dst, xchg, view)
    | None -> (* view empty *)
       (None, View.empty, view)

  (** Respond to gossip exchange

      @param view      TAPS view
      @param xview     explicit view
      @param recvd     entries received from [src]
      @param src       node the entries are received from
      @param me        this node
      @param view_len  max length of TAPS view
      @param xchg_len  max length of gossip exchange
   *)
  let respond ~view ~xview ~recvd ~src ~me ~view_len ~xchg_len =
    let _recvd = recvd and _src = src and _me = me and _view_len = view_len in
    gen_xchg view xview src me xchg_len

  (** Merge received entries into TAPS view

      1. Adjust the trust value of [recvd] entries by multiplying each
         with the trust value of [src]
      2. Remove entries from [recvd] with trust value
         less than the min. trust value in [view]
      3. Select [xchg_len] nodes from the union of [view], [xview], and [recvd],
         based on probabilities influenced by the trust value of each node

      @param view      TAPS view
      @param xview     explicit view
      @param sent      entries sent to [src]
      @param recvd     entries received from [src]
      @param src       node the entries are received from
      @param me        this node
      @param view_len  max length of TAPS view
      @param xchg_len  max length of gossip exchange
   *)
  let merge ~view ~xview ~sent ~recvd ~src ~me ~view_len ~xchg_len =
    let _xchg_len = xchg_len and _sent = sent in
    let recvd = View.remove (Node.id me) recvd in
    let recvd = View.zero_age recvd in
    let recvd = View.adjust_trust (Node.trust src) recvd in
    let recvd = View.filter_trust Config.min_trust recvd in
    let uview = View.union view xview in
    let uview = View.union uview recvd in
    View.weighted_sample view_len uview

end

module S = S
