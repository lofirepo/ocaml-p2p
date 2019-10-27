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

(** View *)

module Rng = Nocrypto.Rng

module Make
         (Node_id: S.NODE_ID)
         (Node : S.NODE with type nid := Node_id.t)
       : S.VIEW with type nid := Node_id.t
                 and type node := Node.t = struct

  module I = struct
    type t = int
    let compare (a: int) b = compare a b
  end

  module IntSet = Set.Make(I)

  module View = Map.Make(Node_id)

  type t = Node.t View.t

  let empty = View.empty

  let add node t =
    let nid = Node.id node in
    View.add nid node t

  let remove = View.remove

  let length = View.cardinal
  let is_empty = View.is_empty

  let zero_age t  =
    View.map
      (fun node -> Node.zero_age node)
      t

  let incr_age t  =
    View.map
      (fun node -> Node.incr_age node)
      t

  (** get oldest node from [view],
    in case there are multiple oldest nodes pick a random one

    returns [Some (nid, node)] or [None] if [view] is empty *)
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

  let mem = View.mem
  let find = View.find_opt

  (** get a random node from [view] *)
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

  (** return [len] random nodes from [view] *)
  let random_subset len t =
    if len < length t then
      begin
        let rset =
          let rec add_rnd rset =
            if IntSet.cardinal rset < len
            then add_rnd @@ IntSet.add (Rng.Int.gen len) rset
            else rset
          in
          add_rnd IntSet.empty in
        let (rview, _) =
          View.fold
            (fun _nid node a ->
              let (rview, n) = a in
              if IntSet.mem n rset
              then (add node rview, n + 1)
              else (rview, n + 1))
            t
            (empty, 0) in
        rview
      end
    else t

  let union t1 t2 =
    View.union
      (fun _nid node1 node2 ->
        if (Node.ver node1) < (Node.ver node2)
        then Some node2
        else Some node1)
      t1 t2

  let filter = View.filter
  let fold = View.fold
  let iter = View.iter
  let map = View.mapi

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
