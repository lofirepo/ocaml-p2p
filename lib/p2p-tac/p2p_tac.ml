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
{1 TAC: Trust-Aware Clustering}

This is an OCaml implementation of a Trust-Aware Clustering protocol
based on the TAC protocol described in the thesis
{{:https://tel.archives-ouvertes.fr/tel-01135867/file/JEGOU_Arnaud.pdf}
Harnessing the power of implicit and explicit socialnetworks through decentralization}.

TAC is a gossip-based protocol that creates and maintains an overlay,
clustering nodes based on similarity and trust information,
thereby creating a similarity-based implicit social network.
The TAPS view serves as the input for the TAC protocol.
 *)

(** Default TAC protocol configuration parameters *)
module Default_config = struct
    let trust_weight = 2.
    let min_trust = 0.1
end

(** Functor building an implementation of TAC
    given a [Node_id], [Node], and gossip [View]. *)
module Make
         (Node_id : P2p.S.NODE_ID)
         (Node : P2p.S.NODE with type nid := Node_id.t)
         (View : P2p.S.VIEW with type nid := Node_id.t
                             and type node := Node.t)
         (Config : S.CONFIG)
       : P2p.S.GOSSIP with type node := Node.t
                       and type view := View.t = struct

  module Rng = Nocrypto.Rng

  (** Select [len] nodes closest to [dst] from [view]
      according to a score based on trust and similarity.

      @param view  TAC view
      @param dst   destination node to send entries to
      @param len   max length of returned entries
      @param e     trust weight, influences the importance of trust vs similarity
                   in the score: sim^(e-2) * trust^e

      @return nodes closest to [dst]
   *)
  let closest ~view ~dst ~len ~e =
    let dlist =
      List.stable_sort
        (fun a b ->
          Float.compare
            ((Float.pow (Node.sim a) (e -. 2.)) *. (Float.pow (Node.trust a) e))
            ((Float.pow (Node.sim b) (e -. 2.)) *. (Float.pow (Node.trust b) e)))
        (List.sort (* random order in case score is even *)
           (fun _a _b -> if Rng.Int.gen 2 = 0 then 1 else -1)
           (View.fold
              (fun _nid node lst ->
                Node.set_sim node (Blip.sim (Node.subs dst) (Node.subs node)) :: lst)
              view [])) in
    List.fold_left
      (fun dview dnode ->
        if View.length dview < len
        then View.add dnode dview
        else dview)
      View.empty
      dlist

  (** Initiate gossip exchange.

      Return selected destination [dst] node to exchange with or None if both [xview] and [view] is empty,
      the [xchg] entries to send, and the updated TAC [view] with increased age

      @param view      TAC view
      @param xview     TAPS view
      @param me        this node
      @param view_len  max length of TAC view
      @param xchg_len  max length of gossip exchange

      @return (dst option, xchg, view)
   *)
  let initiate ~view ~xview ~me ~view_len ~xchg_len  =
    let _view_len = view_len in
    (* view to pick the gossip target from *)
    let rview = if View.is_empty view then xview else view in
    (* pick the oldest entry from the view as the gossip target (destination) *)
    let dst = View.oldest rview in
    match dst with
    | Some dst ->
       let view = View.remove (Node.id dst) view in
       let view = View.inc_age view in
       let uview = View.union view xview in
       let xchg_len = xchg_len - 1 in
       let xchg = closest ~dst:me ~len:xchg_len ~view:uview ~e:Config.trust_weight in
       let xchg = View.add me xchg in
       (Some dst, xchg, view)
    | None -> (* view empty *)
       (None, View.empty, view)

  (** Respond to gossip exchange

      @param view      TAC view
      @param xview     TAPS view
      @param recvd     entries received from [src]
      @param src       node the entries are received from
      @param me        this node
      @param view_len  max length of TAC view
      @param xchg_len  max length of gossip exchange
   *)
  let respond ~view ~xview ~recvd ~src ~me ~view_len ~xchg_len =
    let _recvd = recvd and _src = src and _me = me and _view_len = view_len in
    let uview = View.add me view in
    let uview = View.union uview xview in
    let uview = View.filter_overlap recvd uview in
    closest ~dst:src ~len:xchg_len ~view:uview ~e:Config.trust_weight

  (** Merge received entries into TAC view

      1. Adjust the trust value of [recvd] entries by multiplying each
         with the trust value of [src]
      2. Remove entries from [recvd] with trust value
         less than the min. trust value in [view]
      3. Select [xchg_len] nodes from the union of [view], [xview], and [recvd],
         based on a trust score influenced by the trust value and similarity of each node

      @param view      TAC view
      @param xview     TAPS view
      @param sent      entries sent to [src]
      @param recvd     entries received from [src]
      @param src       node the entries are received from
      @param me        this node
      @param view_len  max length of TAC view
      @param xchg_len  max length of gossip exchange
   *)
  let merge ~view ~xview ~sent ~recvd ~src ~me ~view_len ~xchg_len =
    let _sent = sent in
    (* truncate xchg if it contains more entries than xchg_len *)
    let recvd = View.uniform_sample xchg_len recvd in
    let recvd = View.remove (Node.id me) recvd in
    let recvd = View.zero_age recvd in
    let recvd = View.adjust_trust (Node.trust src) recvd in
    let recvd = View.filter_trust Config.min_trust recvd in
    let uview = View.union view xview in
    let uview = View.union uview recvd in
    closest ~dst:me ~len:view_len ~view:uview ~e:Config.trust_weight
end

module S = S
