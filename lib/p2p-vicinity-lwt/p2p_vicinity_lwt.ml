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
{1 VICINITY with Lwt}

High-level library implementing the VICINITY protocol using Lwt.
*)

(** Functor building an implementation of Vicinity with Lwt
    given a [Node_id], [Node], gossip [View], [Vicinity] implementation,
    and an [Io] event handler module. *)
module Make
         (Node_id : P2p.S.NODE_ID)
         (Node : P2p.S.NODE with type nid := Node_id.t)
         (View : P2p.S.VIEW with type nid := Node_id.t
                             and type node := Node.t)
         (Vicinity : P2p.S.GOSSIP with type node := Node.t
                                   and type view := View.t)
         (Io : S.VICINITY_IO with type nid := Node_id.t
                              and type node := Node.t
                              and type view := View.t)
       : S.VICINITY with type nid := Node_id.t
                     and type node := Node.t
                     and type view := View.t
                     and type io := Io.t = struct

  module Gossip = Vicinity

  type t = {
      me : Node.t;           (** this node *)
      mutable view : View.t; (** partial view of gossip protocol *)
      view_len : int;        (** max view length *)
      xchg_len : int;        (** max exchange length *)
      period : float;        (** gossip period in seconds *)
      io : Io.t;
      mutable stop : unit Lwt.t option;
      mutable stopper : unit Lwt.u option;
    }

  let init ~me ~view ~view_len ~xchg_len ~period ~io =
    { me; view; view_len; xchg_len; period; io;
      stop = None; stopper = None }

  let view t = t.view

  (** wait for [delay] seconds,
      then return the result of thread [t],
      or cancel it if not finished yet **)
  let timeout delay stop t =
    let%lwt _ = Lwt.choose [ Lwt_unix.sleep delay; stop ] in
    match Lwt.state t with
    | Lwt.Sleep    -> Lwt.cancel t; Lwt.return None
    | Lwt.Return v -> Lwt.return (Some v)
    | Lwt.Fail ex  -> Lwt.fail ex

  (** initiate exchange with a node from [t.view],
      wait for response, and return merged view *)
  let initiate t dst sent view =
    match dst with
    | (Some dst) ->
       let%lwt recvd = Io.initiate_gossip t.io dst sent in
       let%lwt recvd = Io.gossip_recvd t.io dst recvd t.view in
       t.view <- Gossip.merge ~view ~xview:View.empty ~view_len:t.view_len
                   ~me:t.me ~src:dst ~sent ~recvd ~xchg_len:t.xchg_len;
       let%lwt _ = Io.view_updated t.io t.me t.view in
       Lwt.return t.view
    | _ ->
       Lwt.return t.view

  (** run initiator: pick a random node from [t.view] to gossip with
      every [t.period] seconds *)
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
              ~view_len:t.view_len ~xchg_len:t.xchg_len in
         let%lwt view_after = timeout t.period stop
                                (initiate t dst sent view_before) in
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

  (** merge received entries from a node and send response *)
  let respond t src recvd =
    let xview = Io.get_xview t.io in
    let sent = Gossip.respond ~view:t.view ~xview ~view_len:t.view_len
                 ~recvd ~src ~me:t.me ~xchg_len:t.xchg_len in
    let%lwt _ = Io.respond_gossip t.io src sent in
    let%lwt recvd = Io.gossip_recvd t.io src recvd t.view in
    t.view <- Gossip.merge ~view:t.view ~xview ~view_len:t.view_len
                ~sent ~recvd ~xchg_len:t.xchg_len ~src ~me:t.me;
    Lwt.return t.view

end

(** Signatures *)
module S = S
