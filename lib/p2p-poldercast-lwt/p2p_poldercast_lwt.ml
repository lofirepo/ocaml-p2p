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
{1 PolderCast with Lwt}

High-level library implementing the PolderCast protocol using Lwt.
*)

(** Functor building an implementation of Poldercast with Lwt
    given a FIXME [Node_id], [Node], gossip [View], PolderCast [Gossip] protocol,
    and an [Io] event handler module. *)
module Make
         (Node_id : P2p.S.NODE_ID)
         (Group_id: P2p.S.GROUP_ID)
         (Node : P2p.S.NODE with type nid := Node_id.t)
         (Group: P2p.S.GROUP with type gid := Group_id.t)
         (View : P2p.S.VIEW with type nid := Node_id.t
                             and type node := Node.t)
         (Msg_id : P2p.S.MSG_ID)
         (Ringcast : P2p.S.GOSSIP_DISSEM with type nid := Node_id.t
                                          and type node := Node.t
                                          and type view := View.t
                                          and type mid := Msg_id.t)
         (Ringcast_io : P2p_ringcast_lwt.S.RINGCAST_IO with type nid := Node_id.t
                                                        and type node := Node.t
                                                        and type view := View.t
                                                        and type mid := Msg_id.t)
         (Ringcast_lwt : P2p_ringcast_lwt.S.RINGCAST with type nid := Node_id.t
                                                      and type node := Node.t
                                                      and type view := View.t
                                                      and type mid := Msg_id.t
                                                      and type io := Ringcast_io.t)
         (Poldercast : P2p_poldercast.S.PUBSUB with type nid := Node_id.t
                                                and type gid := Group_id.t
                                                and type node := Node.t
                                                and type group := Group.t
                                                and type view := View.t
                                                and type mid := Msg_id.t)
       : S.POLDERCAST with type nid := Node_id.t
                       and type gid := Group_id.t
                       and type node := Node.t
                       and type group := Group.t
                       and type view := View.t
                       and type mid := Msg_id.t
                       and type ring := P2p_ringcast_lwt.Make
                                          (Node_id) (Node) (View) (Msg_id)
                                          (Ringcast) (Ringcast_io) .t
                       and type ring_io := Ringcast_io.t = struct

  module Pub = Poldercast.Pub
  module Sub = Poldercast.Sub
  module RCL = P2p_ringcast_lwt.Make (Node_id) (Node) (View) (Msg_id)
                 (Ringcast) (Ringcast_io)

  (** data associated with a subscription *)
  type sub = {
      me : Node.t;
      group : Group.t;
      ring : RCL.t;
      ring_t : unit Lwt.t;
      stop : unit Lwt.t;
      io : Ringcast_io.t;
    }

  type t = {
      me : Node.t;
      view_len : int;
      xchg_len : int;
      period : float;
      fanout : int;
      seen_len : int;
      subs : sub Poldercast.Sub.t; (** Group_id.t -> sub *)
      io : (Group.t -> Ringcast_io.t);
      stop : unit Lwt.t;
      stopper : unit Lwt.u option;
    }

  let sub ?(view=View.empty) t group =
    let io = t.io group in
    let ring = RCL.init
                 ~me:t.me ~view ~view_len:t.view_len
                 ~xchg_len:t.xchg_len ~period:t.period ~fanout:t.fanout
                 ~seen_len:t.seen_len ~io in
    let stop = Lwt.choose [ fst (Lwt.task ()); t.stop ] in
    let ring_t = RCL.run ring ~stop in
    let sub = { me = t.me; group; ring; ring_t; stop; io } in
    let subs = Sub.add group sub t.subs in
    { t with subs }

  let unsub t gid =
    match Sub.find gid t.subs with
    | Some (_group, sub) ->
       let () = Lwt.cancel sub.stop in
       let subs = Sub.remove gid t.subs in
       Ok { t with subs }
    | None -> Error "Group not found"

  let init ~me ?max_subs ~view_len ~xchg_len ~period ~fanout ~seen_len
        ?(sub_list = []) ?stop ~io =
    let subs = match max_subs with
      | Some max_subs -> Sub.init max_subs
      | None -> Sub.empty in
    let (stop, stopper) =
      match stop with
      | None ->
         let (stop, stopper) = Lwt.wait () in
         (stop, Some stopper)
      | Some stop ->
         (stop, None) in
    let t = { me; subs; view_len; xchg_len; period; fanout; seen_len; io;
              stop; stopper } in
    List.fold_left
      (fun t (group, view) ->
        sub t group ~view)
      t sub_list

  let run t =
    t.stop

  let shutdown t =
    match t.stopper with
    | Some stopper ->
       Lwt.wakeup_later stopper ();
    | None ->
       Lwt.cancel t.stop

  let find t gid =
    match Sub.find gid t.subs with
    | Some (_group, sub) ->
       Some sub.ring
    | None -> None

  let view t =
    Sub.fold
      (fun _gid (_group, sub) view ->
        View.union view (RCL.view sub.ring))
      t.subs View.empty

  let to_list t =
    Sub.fold
      (fun _gid data lst ->
        let (group, sub) = data in
        let view = (RCL.view sub.ring) in
        (group, view) :: lst)
      t.subs []

end

(** Signatures *)
module S = S
