(*
  SPDX-FileCopyrightText: 2019 TG x Thoth
  SPDX-License-Identifier: AGPL-3.0-only

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

(** Node ID *)
module type NODE_ID = sig
  type t

  val zero : t
  (** [zero] returns node ID 0 *)

  val one : t
  (** [one] returns node ID 1 *)

  val random : unit -> t
  (** [random] returns a random node ID. *)

  val compare : t -> t -> int
  (** [compare a b] compares node IDs [a] & [b].
      @return
      - [0] if a = b
      - [-1] if [a] < [b]
      - [1] if [b] < [a] *)

  val distance : t -> t -> t
  (** [distance a b] calculates distance between node IDs [a] & [b]. *)

  val distance_ring : t -> t -> (int * t)
  (** [distance_ring a b] calculates distance between node IDs [a] & [b]. *)

  val to_uint64 : t -> Stdint.Uint64.t
  (** [to_uint64 t] returns a uint64 representation of node ID [t] *)

  val to_string : t -> string
  (** [to_string t] returns a string representation of node ID [t] *)

  val pp : Format.formatter -> t -> unit
  (** [pp fmt t] pretty-prints node ID [t] *)
end

(** Group ID *)
module type GROUP_ID = sig
  type t

  val compare : t -> t -> int
  (** [compare a b] compares group IDs [a] & [b].
      @return
      - [0] if a = b
      - [-1] if [a] < [b]
      - [1] if [b] < [a] *)

  val to_string : t -> string
  (** [to_string t] returns a string representation of group ID [t] *)

  val pp : Format.formatter -> t -> unit
  (** [pp fmt t] pretty-prints group ID [t] *)
end

(** Message ID *)
module type MSG_ID = sig
  type t

  val compare : t -> t -> int
  (** [compare a b] compares message IDs [a] & [b].
      @return
      - [0] if a = b
      - [-1] if [a] < [b]
      - [1] if [b] < [a] *)

  val to_string : t -> string
  (** [to_string t] returns a string representation of message ID [t] *)

  val pp : Format.formatter -> t -> unit
  (** [pp fmt t] pretty-prints message ID [t] *)
end

(** Node *)
module type NODE = sig
  type t
  type nid

  val init : ?age:int -> ?ver:int -> nid -> t
  (** [init ?age ?ver nid] *)

  val id : t -> nid
  (** [id t] returns ID of node [t] *)

  val age : t -> int
  (** [age t] returns ID of node profile [t] *)

  val ver : t -> int
  (** [ver t] returns version of node profile [t] *)

  val compare : t -> t -> int
  (** [compare a b] compares IDs of nodes [a] & [b].
      See {! Node_id.compare} *)

  val distance : t -> t -> nid
  (** [distance a b] calculates distance between IDs of nodes [a] & [b].
      See {! Node_id.distance} *)

  val distance_ring : t -> t -> (int * nid)
  (** [distance_ring a b] calculates distance on a ring
      between IDs of nodes [a] & [b].
      See {! Node_id.distance_ring} *)

  val zero_age : t -> t
  (** [zero_age t] sets the age of node profile to zero. *)

  val set_age : t -> int -> t
  (** [set_age t] sets the age of node profile to [age]. *)

  val incr_age : t -> t
  (** [incr_age t] increments the age of node profile. *)

  val set_ver : t -> int -> t
  (** [set_ver t] sets the version of node profile to [ver]. *)

  val incr_ver : t -> t
  (** [incr_ver t] increments the version of node profile. *)

  val to_string : t -> string
  (** [to_string t] returns a string representation of node [t] *)

  val pp : Format.formatter -> t -> unit
  (** [pp fmt t] pretty-prints node [t] *)
end

(** Group *)
module type GROUP = sig
  type t
  type gid
  (* type node *)

  val init : gid -> t
  (** [init gid] initializes a group with ID [gid]. *)

  val id : t -> gid
  (** [id t] returns the ID of group [t]. *)

  val compare : t -> t -> int
  (** [compare a b] compares IDs of groups [a] & [b].
      See {! Group_id.compare} *)

  val pp : Format.formatter -> t -> unit
  (** [pp fmt t] pretty-prints group [t] *)
end

(** Gossip view *)
module type VIEW = sig
  type t
  type nid
  type node

  val empty : t
  (** [empty] is the empty view. *)

  val add : node -> t -> t
  (** [add node t] adds [node] to view [t] *)

  val remove : nid -> t -> t
  (** [remove node t] removes [node] from view [t] *)

  val length : t -> int
  (** [lengh t] returns the number of nodes in view [t] *)

  val is_empty : t -> bool
  (** [is_empty t] returns true if there are no nodes in view [t] *)

  val mem : nid -> t -> bool
  (** [mem nid t] returns true if [nid] is in view [t]. *)

  val find : nid -> t -> node option
  (** [find nid t] returns [Some node] if [nid] is in view [t],
      otherwise [None]. *)

  val oldest : t -> node option
  (** [oldest t] returns the oldest node in view.
      In case there are multiple oldest nodes, picks a random one of those. *)

  val random : t -> node option
  (** [random t] selects a random from view [t]. Returns [None] if [t] is empty. *)

  val random_subset : int -> t -> t
  (** [random_subset n t] selects a random subset of view [t] with [n] elements. *)

  val union : t -> t -> t
  (** [union a b] returns the union of views [a] & [b]. *)

  val zero_age : t -> t
  (** [zero_age t] sets the age of all nodes in view to 0. *)

  val incr_age : t -> t
  (** [incr_age t] increments the age of all nodes in view. *)

  val filter : (nid -> node -> bool) -> t -> t
  val fold : (nid -> node -> 'a -> 'a) -> t -> 'a -> 'a
  val iter : (nid -> node -> unit) -> t -> unit
  val map : (nid -> node -> node) -> t -> t

  val to_list : t -> node list
  val of_list : node list -> t

  val pp : Format.formatter -> t -> unit
  (** [pp ppf t] pretty-prints view [t] *)
end

(** Gossip protocol *)
module type GOSSIP = sig
  type node
  type view

  val initiate :
    view : view
    -> xview : view
               -> me : node
    -> xchg_len : int
    -> (node option * view * view)
  (** [initiate ~view ~xview ~me ~xchg_len ?distance]
      initiates gossip exchange.

      @param ~view current view
      @param ~xview external view from another gossip protocol
      @param ~me this node
      @param ~xchg_len number of items in gossip exchange

      @return [(Some dst, xchg, view)]:
      @param dst destination node
      @param xchg items to send to [dst]
      @param view updated view of this node*)

  val respond :
    view : view
    -> xview : view
    -> recvd : view
    -> src : node
    -> me : node
    -> xchg_len : int
    -> view
  (** [respond ~view ~xview ~recvd ~src ~me ~xchg_len]
      responds to a gossip exchange.

      @param ~view current view
      @param ~xview external view from another gossip protocol
      @param ~recvd received items
      @param ~src source node
      @param ~me this node
      @param ~xchg_len number of items in gossip exchange

      @return updated view *)

  val merge :
    view : view
    -> view_len : int
    -> sent : view
    -> recvd : view
    -> xchg_len : int
    -> me : node
    -> view
  (** [merge ~view ~view_len ~sent ~recvd ~xchg_len ~me]
      merges received entries during a gossip exchange.

      @param ~view current view
      @param ~view_len number of items in [view]
      @param ~sent sent items
      @param ~recvd received items
      @param ~xchg_len number of items in gossip exchange
      @param ~me this node

      @return updated view *)
end

(** Dissemination of messages within a group *)
module type DISSEMINATION = sig
  type nid
  type node
  type view
  type mid
  type seen

  val init_seen : int -> seen
  (** [init_seen len] initializes the queue of last seen messages
      with maximum queue length [len]. *)

  val forward :
    view : view
    -> seen : seen
    -> mid : mid
    -> src : node
    -> me : node
    -> fanout : int
    -> (view * seen)
  (** Select recipients to forward an incoming or outgoing message.
      If [mid] was already [seen] before, the message is not fowarded.

      @param ~view    Current view.
      @param ~seen    Queue of last seen message IDs.
      @param ~mid     Message ID.
      @param ~src     Source node.
      @param ~fanout  Number of nodes to select for forwarding.

      @return [(dsts, seen)]
      @param dsts  The list of destinations to forward to.
      @param seen  Updated seen queue with [mid] added.
    *)
end

(** Gossip + Dissemination combined *)
module type GOSSIP_DISSEM = sig
  type nid
  type node
  type view
  type mid
  type seen

  include GOSSIP
          with type node := node
           and type view := view

  include DISSEMINATION
          with type nid := nid
           and type node := node
           and type view := view
           and type mid := mid
           and type seen := seen
end

(** Subscriptions of this node to groups *)
module type SUBSCRIPTION = sig
  type 'a t
  type gid
  type group

  val empty : 'a t
  (** [empty] returns the empty subscription set. *)

  val add : group -> 'a -> 'a t -> 'a t
  (** [add group 'a t] adds group to the subscription set [t] *)

  val remove : gid -> 'a t -> 'a t
  (** [remove group t] removes group from the subscription set [t] *)

  val length : 'a t -> int
  (** [length t] returns the number of groups in the subscription set. *)

  val is_empty : 'a t -> bool
  (** [is_empty t] returns true if the subscription set is empty. *)

  val mem : gid -> 'a t -> bool
  (** [mem gid t] returns true if [gid] is in the subscription set. *)

  val find : gid -> 'a t -> (group * 'a) option
  (** [find gid t] returns [Some group] if [gid] is in the subscription set,
      otherwise [None]. *)

  val filter : (gid -> (group * 'a) -> bool) -> 'a t -> 'a t
  val fold : (gid -> (group * 'a) -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val iter : (gid -> (group * 'a) -> unit) -> 'a t -> unit
  val map : (gid -> (group * 'a) -> (group * 'a)) -> 'a t -> 'a t

  val to_list : 'a t -> (group * 'a) list
  (** [to_list t] returns the subscription set as list ordered by group ID. *)
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
    include GOSSIP_DISSEM
          with type nid := nid
           and type node := node
           and type view := view
           and type mid := mid
  end

  module Sub : sig
    include SUBSCRIPTION
            with type gid := gid
             and type group := group
  end
end
