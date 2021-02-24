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

open OUnit2
open Stdint

module Node_id = P2p.Node_id
module Group_id = P2p.Group_id
module Node = P2p_poldercast.Node.Make (Node_id)
module Group = P2p.Group.Make (Group_id)
module View = P2p.View.Make (Node_id) (Node)
module Msg_id = P2p.Msg_id
module Ringcast = P2p_ringcast.Make (Node_id) (Node) (View) (Msg_id)
module Poldercast = P2p_poldercast.Make (Node_id) (Group_id) (Node) (Group)
                      (View) (Msg_id) (Ringcast)

let u64 = Uint64.of_int64
let pf = Fmt.pf
let out = Fmt.stdout
let sent_msgs = Hashtbl.create 3

(** union of views *)
module UView = struct

  type t = {
      rings : (Group_id.t, View.t) Hashtbl.t;
    }

  let create =
    { rings = Hashtbl.create 10 }

  let set_ring t gid view =
    Hashtbl.replace t.rings gid view

  let view t =
    Hashtbl.fold
      (fun _gid view uview ->
        View.union uview view)
    t.rings View.empty

end

module Io = struct

  module IoMap = Map.Make (Node)

  type t = {
      group : Group.t;
      me : Node.t;
      uview : UView.t;
      io : (Lwt_io.input_channel * Lwt_io.output_channel) IoMap.t;
    }

  let init me uview =
    (fun group ->
      { group; me; uview; io = IoMap.empty })

  let get_chan t dst =
    match IoMap.find_opt dst t.io with
    | None ->
       let (in_chan, out_chan) = Lwt_io.pipe () in
       (in_chan, out_chan)
    | Some (in_chan, out_chan) ->
       (in_chan, out_chan)

  let recv_gossip t group src xchg =
    let (_in_chan, out_chan) = get_chan t src in
    Lwt_io.write_value out_chan (group, xchg)

  let recv_msg t group src msg =
    let (_in_chan, out_chan) = get_chan t src in
    Lwt_io.write_value out_chan (group, msg)

  (** [initiate_gossip t node xchg]
      sends [xchg] entries to node [dst]
      and returns response *)
  let initiate_gossip t dst xchg =
    pf out "%a/%a # INITIATE_GOSSIP to node %a\n"
      Group.pp t.group
      Node.pp t.me
      Node.pp dst;
    pf out "xchg to send:\n%a\n" View.pp xchg;
    flush stdout;
    let (in_chan, out_chan) = get_chan t dst in
    let%lwt _ = Lwt_io.write_value out_chan (t.group, xchg) in
    let%lwt (rgroup, rxchg) = Lwt_io.read_value in_chan in
    assert_equal t.group rgroup;
    Lwt.return rxchg

  (** [respond_gossip t node xchg]
      sends [xchg] entries in response to node [dst] *)
  let respond_gossip t dst xchg =
    pf out "%a/%a # RESPOND_GOSSIP to node %a\n"
      Group.pp t.group
      Node.pp t.me
      Node.pp dst;
    pf out "xchg to send:\n%a\n" View.pp xchg;
    flush stdout;
    let (_in_chan, out_chan) = get_chan t dst in
    let%lwt _ = Lwt_io.write_value out_chan (t.group, xchg) in
    Lwt.return_unit

  (** [gossip_recvd t node view recvd]
      is called after entries are received during a gossip exchange;
      allows rewriting [recvd] entries with the returned value. *)
  let gossip_recvd _t _src recvd _view =
    Lwt.return recvd

  (** [view_updated node view]
      is called when [view] has been updated after a gossip exchange *)
  let view_updated t node view =
    pf out "%a/%a # VIEW_UPDATED of node %a\n%a\n"
      Group.pp t.group
      Node.pp t.me
      Node.pp node
      View.pp view;
    flush stdout;
    UView.set_ring t.uview (Group.id t.group) view;
    Lwt.return_unit

  let send_msg t dst mid msg =
    pf out "%a/%a # SEND_MSG %a to node %a%s\n"
      Group.pp t.group
      Node.pp t.me
      Msg_id.pp mid
      Node.pp dst
      (Cstruct.to_string msg);
    flush stdout;
    let n =
      match Hashtbl.find_opt sent_msgs mid with
      | Some n -> n
      | None -> 0 in
    Hashtbl.add sent_msgs mid (n + 1);
    Lwt.return_unit

  let get_xview t =
    UView.view t.uview
end

module Ringcast_lwt =
  P2p_ringcast_lwt.Make (Node_id) (Node) (View) (Msg_id) (Ringcast) (Io)

module Poldercast_lwt =
  P2p_poldercast_lwt.Make (Node_id) (Group_id) (Node) (Group) (View) (Msg_id)
    (Ringcast) (Io) (Ringcast_lwt) (Poldercast)

let _ = Nocrypto_entropy_lwt.initialize ()

let () =
  let view_len = 8 in
  let xchg_len = 4 in
  let period = 1.0 in
  let fanout = 5 in
  let seen_len = 10 in

  let me = Node.init (u64 100L) in

  let group1 = Group.init (u64 1000L) in
  let view1 =
    View.add (Node.init (u64 110L))
      (View.add (Node.init (u64 120L))
         (View.add (Node.init (u64 130L))
            (View.add (Node.init (u64 140L))
               (View.add (Node.init (u64 150L))
                  (View.add (Node.init (u64 160L))
                     (View.add (Node.init (u64 170L))
                        View.empty)))))) in

  let group2 = Group.init (u64 2000L) in
  let view2 =
    View.add (Node.init (u64 210L))
      (View.add (Node.init (u64 220L))
         (View.add (Node.init (u64 230L))
            (View.add (Node.init (u64 240L))
               (View.add (Node.init (u64 250L))
                  (View.add (Node.init (u64 260L))
                     (View.add (Node.init (u64 270L))
                        View.empty)))))) in

  let io = Io.init me UView.create in
  let sub_list = [ (group1, view1); (group2, view2) ] in
  let pc = Poldercast_lwt.init ~me ~view_len ~xchg_len
             ~period ~fanout ~seen_len ~sub_list ~io
             ?max_subs:None ?stop:None in

  let timeout = Lwt_unix.sleep 5.5 in
  Lwt_main.run @@
    Lwt.choose [ Poldercast_lwt.run pc;
                 Lwt.map (fun () -> Poldercast_lwt.shutdown pc) timeout ];
