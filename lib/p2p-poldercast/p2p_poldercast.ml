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
{1 PolderCast: P2P Topic-based Pub/Sub}

This is an OCaml implementation of PolderCast,
a P2P topic-based pub/sub protocol described in the paper
{{:https://hal.inria.fr/hal-01555561} PolderCast: Fast, Robust, and Scalable Architecture for P2P Topic-Based Pub/Sub}

It relies on three different gossip protocols:
- Random Peer Sampling, to find random nodes in the network. See the {!P2p_cyclon}module.
- Clustering, to find nodes with overlapping subscriptions. See the {!P2p_vicinity} module.
- Dissemination, for the dissemination of messages within a topic. See the {!P2p_ringcast} module.

{2 Privacy}

PolderCast, as specified in the paper, employs VICINITY for clustering,
and transmits full node subscription profiles in the clear.
See {{:https://github.com/p2pcollab/ocaml-blip} BLIP} for a privacy mechanism
that transmits subscriptions as randomized Bloom filters instead,
and {{:https://github.com/p2pcollab/ocaml-psi} BFPSI},
a Bloom filter-based Private Set Intersection protocol
to determine common subscriptions of two nodes.
*)

(** PolderCast*)
module Make
         (Node_id: P2p.S.NODE_ID)
         (Group_id: P2p.S.GROUP_ID)
         (Node: S.NODE with type nid := Node_id.t)
         (Group: P2p.S.GROUP with type gid := Group_id.t)
         (View: P2p.S.VIEW with type nid := Node_id.t
                            and type node := Node.t)
         (Msg_id: P2p.S.MSG_ID)
         (Pub : P2p.S.GOSSIP_DISSEM with type nid := Node_id.t
                                     and type node := Node.t
                                     and type view := View.t
                                     and type mid := Msg_id.t)
       : S.PUBSUB with type nid := Node_id.t
                   and type node := Node.t
                   and type view := View.t
                   and type mid := Msg_id.t
                   and type gid := Group_id.t
                   and type group := Group.t = struct

  (** Publishing *)
  module Pub = Pub

  (** Subscriptions of this node *)
  module Sub = struct
    module Subs = Map.Make (Group_id)

    type group = Group.t

    type 'a t = {
        subs : (group * 'a) Subs.t; (** subscriptions: gid -> group *)
        bf : Group_id.t Bloomf.t;
        mutable bf_dirty : bool;
        b : Bitv.t;
      }

    let init max_subs =
      let subs = Subs.empty in
      let bf = Bloomf.create max_subs in
      let b = Bloomf.bits bf in
      { subs; bf; b; bf_dirty = false }

    let empty =
      init 1000

    let rebuild_bf t =
      Bloomf.clear t.bf;
      Subs.iter (fun gid _group -> Bloomf.add t.bf gid) t.subs;
      t.bf_dirty <- false

    (** subscribe to [group] with ID [gid] *)
    let add group data t =
      let gid = Group.id group in
      let subs = Subs.add gid (group, data) t.subs in
      Bloomf.add t.bf gid;
      { t with subs }

    let remove gid t =
      let subs = Subs.remove gid t.subs in
      { t with subs; bf_dirty = true }

    let bloom t =
      if t.bf_dirty then rebuild_bf t;
      Bloomf.bits t.bf

    let blip t e =
      let _m, k = Bloomf.params t.bf in
      Blip.flip (bloom t) (Blip.p e k)

    let length t =
      Subs.cardinal t.subs

    let is_empty t =
      Subs.is_empty t.subs

    let mem gid t =
      Subs.mem gid t.subs

    let find gid t =
      Subs.find_opt gid t.subs

    let filter f t =
      let subs = Subs.filter f t.subs in
      { t with subs }

    let fold f t a =
      Subs.fold f t.subs a

    let iter f t =
      Subs.iter f t.subs

    let map f t =
      let subs = Subs.mapi f t.subs in
      { t with subs }

    let to_list t =
      List.map
        (fun (_gid, group) -> group)
        (Subs.bindings t.subs)

    let of_list max_subs l =
      List.fold_left
        (fun t (group, data) ->
          add group data t)
        (init max_subs) l

  end

end

(** Signatures *)
module S = S

(** Node *)
module Node = Node
