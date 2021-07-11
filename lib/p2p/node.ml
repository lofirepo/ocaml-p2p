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

module Make
         (Node_id : S.NODE_ID)
       : S.NODE with type nid := Node_id.t = struct

  type nid = Node_id.t

  type t = {
      id : nid; (* node ID: Curve25519 public key *)
      age : int; (* age: number of gossip rounds since this entry is in view *)
      version : int; (* profile version: newer versions replace old ones *)
      trust : float; (* trust value between [0,1] *)
      known : bool; (* node is known locally, with explicitly set trust value *)
      subs : Bitv.t; (* Bloom filter of subscriptions *)
      sim : float; (* similarity metric between [0,1] *)
      signature : Bytes.t; (* signature over the rest of the fields
                              by the node's private key *)
    }

  let id t = t.id
  let age t = t.age
  let version t = t.version
  let trust t = t.trust
  let known t = t.known
  let subs t = t.subs
  let sim t = t.sim
  let signature t = t.signature

  let init ?(age=0) ?(version=0) ?(trust=0.1) ?(known=false)
        ?(subs=(Bitv.create 0 false)) ?(sim=0.) ?(signature=Bytes.empty) id =
    { id; age; version; trust; known; subs; sim; signature }

  let compare a b =
    Node_id.compare a.id b.id

  let distance a b =
    Node_id.distance a.id b.id

  let distance_ring a b =
    Node_id.distance_ring a.id b.id

  let zero_age t =
    { t with age = 0 }

  let inc_age t =
    { t with age = t.age + 1 }

  let set_age t age =
    { t with age }

  let adjust_trust t r =
    { t with trust = t.trust *. r }

  let set_trust t trust =
    { t with trust }

  let set_known t known =
    { t with known }

  let set_subs t subs =
    { t with subs }

  let set_sim t sim =
    { t with sim }

  let set_signature t signature =
    { t with signature }

  let inc_version t =
    { t with version = t.version + 1 }

  let set_version t version =
    { t with version }

  let to_string t =
    Printf.sprintf "%s (version: %d; age: %d; trust: %.3f; known: %b; sim: %.3f)"
      (Node_id.to_string t.id) t.version t.age t.trust t.known t.sim

  let pp ppf t =
    Fmt.pf ppf "%s (version: %d; age: %d; trust: %.3f; known: %b; sim: %.3f)\n"
      (Node_id.to_string t.id) t.version t.age t.trust t.known t.sim

end
