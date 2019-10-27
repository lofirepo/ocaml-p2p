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
{1 RingCast with Lwt}

High-level library implementing the RingCast protocol using Lwt.
*)

(** Functor building an implementation of Ringcast with Lwt
    given a [Node_id], [Node], gossip [View], RingCast [Gossip] protocol,
    and an [Io] event handler module. *)
module Make
         (Node_id : P2p.S.NODE_ID)
         (Node : P2p.S.NODE with type nid := Node_id.t)
         (View : P2p.S.VIEW with type nid := Node_id.t
                             and type node := Node.t)
         (Msg_id : P2p.S.MSG_ID)
         (Ringcast : P2p.S.GOSSIP_DISSEM with type nid := Node_id.t
                                          and type node := Node.t
                                          and type view := View.t
                                          and type mid := Msg_id.t)
         (Io : S.RINGCAST_IO with type nid := Node_id.t
                              and type node := Node.t
                              and type view := View.t
                              and type mid := Msg_id.t)
       : S.RINGCAST with type nid := Node_id.t
                     and type node := Node.t
                     and type view := View.t
                     and type mid := Msg_id.t
                     and type io := Io.t = struct

  module Gossip = Ringcast

  type t = {
      me : Node.t;           (** this node *)
      mutable view : View.t; (** partial view of gossip protocol *)
      view_len : int;        (** max view length *)
      xchg_len : int;        (** max exchange length *)
      period : float;        (** gossip period in seconds *)
      fanout : int;
      io : Io.t;
      mutable seen : Gossip.seen;
      mutable stop : unit Lwt.t option;
      mutable stopper : unit Lwt.u option;
    }

  let init ~me ~view ~view_len ~xchg_len ~period ~fanout ~seen_len ~io =
    let seen = Gossip.init_seen seen_len in
    { me; view; view_len; xchg_len; period; fanout; seen; io;
      stop = None; stopper = None; }

  let view t = t.view

  (** Wait for [delay] seconds,
      then return the result of thread [t],
      or cancel it if not finished yet **)
  let timeout delay stop t =
    let%lwt _ = Lwt.choose [ Lwt_unix.sleep delay; stop ] in
    match Lwt.state t with
    | Lwt.Sleep    -> Lwt.cancel t; Lwt.return None
    | Lwt.Return v -> Lwt.return (Some v)
    | Lwt.Fail ex  -> Lwt.fail ex

  (** Initiate exchange with a node from [t.view],
      wait for response, and return merged view. *)
  let initiate t dst sent view =
    match dst with
    | (Some dst) ->
       let%lwt recvd = Io.initiate_gossip t.io dst sent in
       let%lwt recvd = Io.gossip_recvd t.io dst recvd t.view in
       t.view <- Gossip.merge ~view ~view_len:t.view_len ~me:t.me
                   ~sent ~recvd ~xchg_len:t.xchg_len;
       let%lwt _ = Io.view_updated t.io t.me t.view in
       Lwt.return t.view
    | _ ->
       Lwt.return t.view

  (** Run active thread.
      Pick a random node from [t.view] to gossip with
      every [t.period] seconds. *)
  let run ?stop t =
    match t.stop with
    | Some stop -> stop
    | None ->
       let stop =
         match stop with
         | None ->
            let (stop, stopper) = Lwt.wait () in
            t.stopper <- Some stopper;
            t.stop <- Some stop;
            stop
         | Some stop ->
            t.stop <- Some stop;
            stop in

    let rec loop () =
      let xview = Io.get_xview t.io in
      let (dst, sent, view_before) =
        Gossip.initiate ~me:t.me ~view:t.view ~xview
          ~xchg_len:t.xchg_len in
      let%lwt view_after = timeout t.period stop (initiate t dst sent view_before) in
      let _ = t.view <- match view_after with
                        | Some v -> v
                        | _ -> view_before in
      match Lwt.state stop with
      | Lwt.Sleep -> loop ()
      | _ -> Lwt.return_unit
    in loop ()

  let shutdown t =
    match t.stopper with
    | Some stopper ->
       Lwt.wakeup_later stopper ();
       t.stop <- None;
       t.stopper <- None
    | None ->
       match t.stop with
       | Some stop ->
          Lwt.cancel stop;
          t.stop <- None
       | None -> ()

  (** Merge received entries from a node and send response *)
  let respond t src recvd =
    let xview = Io.get_xview t.io in
    let sent = Gossip.respond ~view:t.view ~xview
                 ~recvd ~src ~me:t.me ~xchg_len:t.xchg_len in
    let%lwt _ = Io.respond_gossip t.io src sent in
    let%lwt recvd = Io.gossip_recvd t.io src recvd t.view in
    t.view <- Gossip.merge ~view:t.view ~view_len:t.view_len
                ~sent ~recvd ~xchg_len:t.xchg_len ~me:t.me;
    Lwt.return t.view

  (** Forward [msg] with ID [mid] from [src] to [t.fanout] nodes in [t.view] *)
  let forward t src mid msg =
    let (targets, seen) =
      Gossip.forward ~view:t.view ~seen:t.seen ~mid
        ~src ~me:t.me ~fanout:t.fanout
    in
    View.iter
      (fun _nid node ->
        let _ =
          let%lwt _ = Io.send_msg t.io node mid msg
          in Lwt.return_unit
        in ()) targets;
    t.seen <- seen;
    Lwt.return_unit

end

(** Signatures *)
module S = S
