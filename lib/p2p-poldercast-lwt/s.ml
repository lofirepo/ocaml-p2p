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

module type POLDERCAST = sig
  type t
  type nid
  type gid
  type node
  type group
  type view
  type mid
  type ring
  type ring_io

  val init : me:node -> max_subs:int -> view_len:int -> xchg_len:int
             -> period:float -> fanout:int -> seen_len:int
             -> sub_list:(group * view) list -> stop:unit Lwt.t option
             -> io:(group -> ring_io) -> t
  (** [init node view view_len xchg_len period fanout seen_cap]
    initializes a PolderCast instance with the following configuration.

    @param me        this node
    @param view      initial view
    @param view_len  max view length
    @param xchg_len  number of entries to exchange at each period
    @param period    gossip period, in seconds
    @param fanout    dissemination fanout
    @param seen_len  length of queue of last seen message IDs
    @param stop      thread that shuts down all groups when cancelled
   *)

  val run : t -> unit Lwt.t
  (** [run t] returns a promise that is fulfilled upon [shutdown] *)

  val shutdown : t -> unit
  (** [shutdown t] shuts down all subscribed groups.

      After calling this function, [sub] cannot be called again with [t]. *)

  val sub : view:view -> t -> group -> t
  (** [sub ?view t group] subscribes to [group] with initial [view] *)

  val unsub : t -> gid -> (t, string) result
  (** [unsub t gid] unsubscribes from group with ID [gid] *)

  val find : t -> gid -> ring option

  val view : t -> view
  (** [view t] returns the combined view of all subscribed groups *)

  val to_list : t -> (group * view) list
  (** [to_list t] returns a list of [(group, view)] pairs
      of all subscribed groups paired with their current membership view *)
end

module type RINGCAST_IO = sig
  include P2p_ringcast_lwt.S.RINGCAST_IO
  type group

  val init : 'a -> group -> t
end
