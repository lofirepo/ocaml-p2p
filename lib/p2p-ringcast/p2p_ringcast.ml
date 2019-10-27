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
{1 RingCast: P2P hybrid dissemination protocol}

This is an OCaml implementation of RingCast,
a P2P hybrid (probabilistic/deterministic) dissemination protocol
described in the paper
{{:https://hal.inria.fr/hal-01555561} PolderCast}
(and earlier in
{{:https://www.distributed-systems.net/my-data/papers/2007.mw.pdf}
Hybrid Dissemination}.

It organizes nodes in a bidirectional ring structure
and forwards messages to neighbours as well as random nodes.
It achieves complete dissemination of messages with a low message overhead.
*)

(** Functor building an implementation of Ringcast
    given a Node_id, Node, gossip View, and Msg_id. *)
module Make
         (Node_id: P2p.S.NODE_ID)
         (Node: P2p.S.NODE with type nid := Node_id.t)
         (View: P2p.S.VIEW with type nid := Node_id.t
                        and type node := Node.t)
         (Msg_id: P2p.S.MSG_ID)
       : S.GOSSIP_DISSEM with type nid := Node_id.t
                          and type node := Node.t
                          and type view := View.t
                          and type mid := Msg_id.t = struct

  module W = struct
    type t = int
    let weight _ = 1
  end

  module Rng = Nocrypto.Rng
  module SeenQ = Lru.F.Make(Msg_id)(W)

  type seen = SeenQ.t

  type dist =
    {
      node: Node.t;
      dir: int;
      dist: Node_id.t;
    }

  (** selects [len] closest neighbours to [dst] from [view]
      (len/2 with lower and len/2 with higher ID) *)
  let closest ~view ~dst ~len =
    let dlist =
      List.stable_sort
        (fun n1 n2 ->
          Node_id.compare n1.dist n2.dist)
        (List.sort
           (fun _n1 _n2 -> if Rng.Int.gen 2 = 0 then 1 else -1)
           (View.fold
              (fun _nid node lst ->
                let (dir, dist) = Node.distance_ring dst node in
                { node; dir; dist } :: lst)
              view [])) in
    let (dview_lo, dview_hi) =
      List.fold_left
        (fun (dview_lo, dview_hi) dnode ->
          let dview_lo =
            if dnode.dir < 0 then
              if View.length dview_lo < len / 2 then
                View.add dnode.node dview_lo
              else dview_lo
            else dview_lo in
          let dview_hi =
            if 0 < dnode.dir then
              if View.length dview_hi < len / 2 then
                View.add dnode.node dview_hi
              else dview_hi
            else dview_hi in
          (dview_lo, dview_hi))
        (View.empty, View.empty)
        dlist
    in
    View.union dview_lo dview_hi

  (** find the closest neighbour with lower ID to [dst] from [view] *)
  let predecessor ~view ~dst =
    let dlist =
      List.stable_sort
        (fun n1 n2 ->
          compare n1.dist n2.dist)
        (List.sort
           (fun _n1 _n2 -> if Rng.Int.gen 2 = 0 then 1 else -1)
           (View.fold
              (fun _nid node lst ->
                let (dir, dist) = Node.distance_ring dst node in
                { node; dir; dist } :: lst)
              view [])) in
    match
      List.nth_opt
        (List.filter
           (fun dnode -> dnode.dir < 0)
           dlist)
        0 with
    | Some dnode -> Some dnode.node
    | None -> None

  (** find the closest neighbour with lower ID to [dst] from [view] *)
  let successor ~view ~dst =
    let dlist =
      List.stable_sort
        (fun n1 n2 ->
          compare n1.dist n2.dist)
        (List.sort
           (fun _n1 _n2 -> if Rng.Int.gen 2 = 0 then 1 else -1)
           (View.fold
              (fun _nid node lst ->
                let (dir, dist) = Node.distance_ring dst node in
                { node; dir; dist } :: lst)
              view [])) in
    match
      List.nth_opt
        (List.filter
           (fun dnode -> 0 < dnode.dir)
           dlist)
        0 with
    | Some dnode -> Some dnode.node
    | None -> None

  let initiate ~view ~xview ~me ~xchg_len =
    let dst = View.oldest view in
    match dst with
    | Some dst ->
       let view = View.remove (Node.id dst) view in
       let view = View.incr_age view in
       let uview = View.union view xview in
       let xchg = closest ~dst:me ~len:xchg_len ~view:uview in
       let xchg = View.random_subset (xchg_len-1) xchg in
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
    closest ~dst:src ~len:xchg_len ~view:uview

  let merge ~view ~view_len ~sent ~recvd ~xchg_len ~me =
    let _sent = sent in
    let recvd = View.remove (Node.id me) recvd in
    let recvd = View.random_subset xchg_len recvd in
    let recvd = View.zero_age recvd in
    let uview = View.union view recvd in
    closest ~dst:me ~len:view_len ~view:uview

  let init_seen seen_len =
    SeenQ.empty seen_len

  let forward ~view ~seen ~mid ~src ~me ~fanout =
    if SeenQ.mem mid seen then (* already seen *)
      (View.empty, seen)
    else
      let dsts =
        let d = Node.compare me src in
        let fanout_rnd =
          if d = 0 then fanout - 2 (* msg from self *)
          else fanout - 1 in
        let dsts = View.random_subset fanout_rnd view in
        if 0 < d then (* msg from successor, fwd to predecessor *)
          let dst = predecessor ~view ~dst:me in
          match dst with
          | Some node -> View.add node dsts
          | _ -> dsts
        else if d < 0 then (* msg from predecessor fwd to successor *)
          let dst = successor ~view ~dst:me in
          match dst with
          | Some node -> View.add node dsts
          | _ -> dsts
        else (* msg from self, fwd to both successor & predecessor *)
          let pnode = predecessor ~view ~dst:me in
          let snode = successor ~view ~dst:me in
          let dsts =
            match pnode with
            | Some node -> View.add node dsts
            | _ -> dsts in
          match snode with
          | Some node -> View.add node dsts
          | _ -> dsts in
      let seen =
        SeenQ.trim @@
          SeenQ.add mid 1 seen in
      (dsts, seen)

end

(** Signatures *)
module S = S
