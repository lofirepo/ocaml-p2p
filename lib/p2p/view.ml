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

(** Gossip view *)

module Rng = Nocrypto.Rng

module Make
         (Node_id: S.NODE_ID)
         (Node : S.NODE with type nid := Node_id.t)
       : S.VIEW with type nid := Node_id.t
                 and type node := Node.t = struct

  let min x y =
    if x < y then x else y

  let max x y =
    if x < y then y else x

  module I = struct
    type t = int
    let compare (a: int) b = compare a b
  end

  module IntSet = Set.Make(I)

  module View = Map.Make(Node_id)

  type t = Node.t View.t

  let empty = View.empty

  let mem = View.mem
  let find = View.find_opt

  let length = View.cardinal
  let is_empty = View.is_empty

  let filter = View.filter
  let fold = View.fold
  let iter = View.iter
  let map = View.mapi

  let remove = View.remove

  let add node t =
    let nid = Node.id node in
    View.add nid node t

  let zero_age t  =
    View.map
      (fun node -> Node.zero_age node)
      t

  let inc_age t  =
    View.map
      (fun node -> Node.inc_age node)
      t

  let adjust_trust r t  =
    View.map
      (fun node ->
        let node = Node.set_known node false in
        Node.adjust_trust node r)
      t

  let filter_trust min_trust t  =
    View.filter
      (fun _nid node -> min_trust <= Node.trust node)
      t

  let filter_overlap view t  =
    View.filter
      (fun nid _node -> not (View.mem nid view))
      t

  (** Return the minimum trust value in [t] *)
  let min_trust t =
      View.fold
        (fun _nid node min_trust ->
          min min_trust (Node.trust node))
        t 2.0

  (** Return the maximum trust value in [t] *)
  let max_trust t =
      View.fold
        (fun _nid node max_trust ->
          max max_trust (Node.trust node))
        t (-1.0)

  (** Return the sum of all trust values in [t] *)
  let sum_trust t =
      View.fold
        (fun _nid node sum ->
          sum +. Node.trust node)
        t 0.0

  (** Get the oldest node from [view].
      In case there are multiple oldest nodes pick a random one.
      Returns [Some (nid, node)] or [None] if [view] is empty *)
  let oldest t =
    match
      View.fold
        (fun nid node oldest ->
          match oldest with
          | None ->
             Some (nid, node, 1)
          | Some (_onid, onode, _n) when (Node.age onode) < (Node.age node) ->
             Some (nid, node, 1)
          | Some (onid, onode, n) when (Node.age onode) = (Node.age node) ->
             let n = n + 1 in
             let max = 1000 in
              if float_of_int (Rng.Int.gen max) /. float_of_int max
                < 1. /. float_of_int n
             then Some (nid, node, n)
             else Some (onid, onode, n)
          | _ -> oldest)
        t None
    with
    | Some (_nid, node, _n) -> Some node
    | None -> None

  (** Return a random node from [view] *)
  let random t =
    let len = length t in
    if 0 < len then
      let r = Rng.Int.gen len in
      let (rnode, _) =
        View.fold
          (fun _nid node a ->
            let (rnode, n) = a in
            if (n = r)
            then (Some node, n + 1)
            else (rnode, n + 1))
          t
           (None, 0)
      in
      rnode
    else
      None

  (* return a random number between ]0,1]  *)
  let rnd () =
    Float.of_int ((Rng.Int.gen 1000) + 1) /. 1000.

  (** Return a uniformly random sample of nodes of length [len] from [view] *)
  let uniform_sample len t =
    if len < length t then
      begin
        let wlist =
          View.fold
            (fun _nid node lst ->
              (node, (0. -. (Float.log (rnd ()))) /. (Node.trust node)) :: lst)
            t [] in
        let swlist =
          List.fast_sort
            (fun a b -> Float.compare (snd a) (snd b))
            wlist in
        let rview =
          let rec add_head rview swlist =
            if length rview < len then
              match swlist with
              | hd :: tl -> add_head (add (fst hd) rview) tl
              | [] -> rview
            else rview
          in
          add_head View.empty swlist in
        rview
      end
    else t

  (** Return a weighted random sample of nodes of length [len] from [view],
      weighted by their trust values *)
  let weighted_sample len t =
     if len < length t then
      begin
        let wlist =
          View.fold
            (fun _nid node lst ->
              (node, (0. -. (Float.log (rnd ()))) /. (Node.trust node)) :: lst)
            t [] in
        let swlist =
          List.fast_sort
            (fun a b -> Float.compare (snd a) (snd b))
            wlist in
        let rview =
          let rec add_head rview swlist =
            if length rview < len then
              match swlist with
              | hd :: tl -> add_head (add (fst hd) rview) tl
              | [] -> rview
            else rview
          in
          add_head View.empty swlist in
        rview
      end
     else t

  (** Return the union of two views
      In case of duplicates keep the entry with the newer version,
      and set the trust value to the local trust value if available
      or otherwise the larger trust value of the two *)
  let union t1 t2 =
    View.union
      (fun _nid node1 node2 ->
        let newer_node =
          if (Node.version node1) < (Node.version node2)
          then node2 else node1 in
        let trust =
          if (Node.known node1) then (Node.trust node1)
          else if (Node.known node2) then (Node.trust node2)
          else max (Node.trust node1) (Node.trust node2) in
        Some (Node.set_trust newer_node trust))
      t1 t2

  (** Choose a view at random from two views.
      If one of the views is empty, the other one is returned always. *)
  let choose_view x y =
    if View.is_empty x
    then y
    else if View.is_empty y
    then x
    else if 0 = Rng.Int.gen 1 then x else y

  let to_list t =
    List.map
      (fun (_nid, node) -> node)
      (View.bindings t)

  let of_list l =
    List.fold_left
      (fun t node ->
        add node t)
      empty l

  let pp ppf t =
    View.iter (fun _nid node -> Fmt.pf ppf " - %a\n" Node.pp node) t
end
