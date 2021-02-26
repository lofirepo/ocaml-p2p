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

module type RINGCAST = sig
  type t
  type nid
  type node
  type view
  type mid
  type io

  val init : me:node -> view:view -> view_len:int -> xchg_len:int -> period:float
             -> fanout:int -> seen_len:int -> io:io -> t
  (** [init node view view_len xchg_len period fanout seen_cap]
    initializes a RingCast instance with the following configuration.

    @param me        this node
    @param view      initial view
    @param view_len  max view length
    @param xchg_len  number of entries to exchange at each period
    @param period    gossip period, in seconds
    @param fanout    dissemination fanout
    @param seen_len  length of queue of last seen message IDs
   *)

  val run : ?stop:unit Lwt.t -> t -> unit Lwt.t
  (** [run ?stop t] runs initiator thread:
      picks a random node from [view] to gossip with
      every [period] seconds.

      If [?stop] is provided, this initiator thread returns
      as soon as the [stop] thread is fulfilled. *)

  val shutdown : t -> unit
  (** [shutdown t] stops initiator thread.

   In case [run] was called with a [stop] argument,
   the [stop] thread is cancelled, otherwise it is fulfilled. *)

  val respond : t -> node -> view -> view Lwt.t
  (** [respond t node recvd]
      merges received entries from a node and sends response *)

  val forward : t -> node -> mid -> Cstruct.t -> unit Lwt.t
  (** [forward t src mid msg]
      receives a message ([msgid, msg]) from a [src] node and
      selects nodes from the local view it should be forwarded to
      (predecessor or successor on the ring and some random nodes,
      limited by [fanout]);
      calls [Io.send_msg] for each selected node *)

  val view : t -> view
  (** [view t] returns current view *)
end

module type RINGCAST_IO = sig
  type t
  type nid
  type mid
  type node
  type view

  val initiate_gossip : t -> node -> view -> view Lwt.t
  (** [initiate_gossip t node xchg]
      sends [xchg] entries to [node]
      and returns response *)

  val respond_gossip : t -> node -> view -> unit Lwt.t
  (** [respond_gossip t node xchg]
      sends [xchg] entries in response to [node] *)

  val gossip_recvd : t -> node -> view -> view -> view Lwt.t
  (** [gossip_recvd t src recvd view]
      is called after entries are received during an exchange;
      allows rewriting [recvd] entries with the returned value. *)

  val view_updated : t -> node -> view -> unit Lwt.t
  (** [view_updated t node view]
      is called when [view] has been updated
      after a gossip exchange with [node] *)

  val get_xview : t -> view
  (** [get_xview t]
      is called before selecting entries for a gossip exchange
      and should return the view of other gossip protocol(s), if any. *)

  val send_msg : t -> node -> mid -> Cstruct.t -> unit Lwt.t
  (** [send_msg t dst msgid msg] *)
end
