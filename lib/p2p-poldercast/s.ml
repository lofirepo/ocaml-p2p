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

(** Node *)
module type NODE = sig

  include P2p.S.NODE

  (** Subscriptions of node *)
  val subs : t -> Bitv.t

  val set_subs : t -> Bitv.t -> t

  val sim : t -> t -> float
end

(** Subscriptions of this node to groups *)
module type SUBSCRIPTION = sig

  include P2p.S.SUBSCRIPTION

  val init : int -> 'a t
  (** [int max_subs] initializes an empty subscription set
      with [max_subs] maximum number of subscriptions expected. *)

  val of_list : int -> (group * 'a) list -> 'a t
  (** [of_list max_subs sub_list] initializes a subscription set
      from a list of (group, metadata) pairs *)

  val bloom : 'a t -> Bitv.t
  (** [bloom t] returns a bloom filter with the subscribed group IDs inserted. *)

  val blip : 'a t -> float -> Bitv.t
  (** [blip t e] is [bloom] with independent random bit flips.

      @param e  Differential privacy parameter ε.
                Determines the privacy-utility trade-off.
                See {! Blip.p} in the documentation of the [Blip] module. *)
end

(** Publish-subscribe message dissemination *)
module type PUBSUB = sig
  type nid
  type node
  type view
  type mid
  type gid
  type group

  module Pub : sig
    include P2p.S.GOSSIP_DISSEM
          with type nid := nid
           and type node := node
           and type view := view
           and type mid := mid
  end

  module Sub : SUBSCRIPTION
         with type gid := gid
          and type group := group
end
