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

open Stdint

let u64 = Uint64.of_int64
let pf = Fmt.pf
let out = Fmt.stdout

module Node_id = P2p.Node_id

(* FIXME
module Make_node (Node_id: P2p.S.NODE_ID)
       : P2p.S.NODE with type nid := Node_id.t = struct

  module Node = P2p.Node.Make (Node_id)
  include Node

  (** 1 / (a - b) *)
  let sim a b =
    1. /. Uint64.to_float
            (Node_id.to_uint64
               (Node_id.distance (Node.id a) (Node.id b)))
end
 *)

module Node = P2p.Node.Make (Node_id)
module View = P2p.View.Make (Node_id) (Node)
module Vicinity = P2p_vicinity.Make (Node_id) (Node) (View)

module Io = struct

  type t = {
      node_id: Node_id.t;
      in_chan : Lwt_io.input_channel;
      out_chan : Lwt_io.output_channel;
    }

  let init node_id in_chan out_chan =
    { node_id; in_chan; out_chan }

  (** [initiate_gossip t node xchg]
      sends [xchg] entries to node [dst]
      and returns response *)
  let initiate_gossip t dst xchg =
    pf out "%a # INITIATE_GOSSIP to node %a\n" Node_id.pp t.node_id Node.pp dst;
    pf out "xchg to send:\n%a\n" View.pp xchg;
    flush stdout;
    let%lwt _ = Lwt_io.write_value t.out_chan xchg in
    Lwt_io.read_value t.in_chan

  (** [respond_gossip t node xchg]
      sends [xchg] entries in response to node [dst] *)
  let respond_gossip t dst xchg =
    pf out "%a # RESPOND_GOSSIP to node %a\n" Node_id.pp t.node_id Node.pp dst;
    pf out "xchg to send:\n%a\n" View.pp xchg;
    flush stdout;
    let%lwt _ = Lwt_io.write_value t.out_chan xchg in
    Lwt.return_unit

  (** [gossip_recvd t node view recvd]
      is called after entries are received during a gossip exchange;
      allows rewriting [recvd] entries with the returned value. *)
  let gossip_recvd _t _src recvd _view =
    Lwt.return recvd

  (** [view_updated node view]
      is called when [view] has been updated after a gossip exchange *)
  let view_updated t node view =
    pf out "%a # VIEW_UPDATED of node %a\n%a\n" Node_id.pp t.node_id Node.pp node View.pp view;
    flush stdout;
    Lwt.return_unit

  let get_xview _t =
    View.empty
end

module Vicinity_lwt =
  P2p_vicinity_lwt.Make (Node_id) (Node) (View) (Vicinity) (Io)

let rec read_chan ch vc node rnode =
  let%lwt recvd = Lwt_io.read_value ch in
  pf out "%a # READ_CHAN\n" Node_id.pp (Node.id node);
  pf out "recvd:\n%a\n" View.pp recvd;
  flush stdout;
  let%lwt view = Vicinity_lwt.respond vc rnode recvd in
  pf out "recvd:\n%a\n" View.pp recvd;
  pf out "view:\n%a\n" View.pp view;
  flush stdout;
  read_chan ch vc node rnode

let _ = Nocrypto_entropy_lwt.initialize ()

let () =
  let view_len = 8 in
  let xchg_len = 4 in
  let period = 1.0 in

  let (in_ch1, out_ch2) = Lwt_io.pipe () in
  let (in_ch2, out_ch1) = Lwt_io.pipe () in

  let node1 = Node.init (u64 100L) in
  let io1 = Io.init (Node.id node1) in_ch1 out_ch1 in
  let view1 =
    View.add (Node.init (u64 110L))
      (View.add (Node.init (u64 120L))
         (View.add (Node.init (u64 130L))
            (View.add (Node.init (u64 140L))
               (View.add (Node.init (u64 150L))
                  (View.add (Node.init (u64 160L))
                     (View.add (Node.init (u64 170L))
                        View.empty)))))) in
  let vc1 = Vicinity_lwt.init ~me:node1 ~view:view1 ~view_len ~xchg_len ~period ~io:io1 in

  let node2 = Node.init (u64 200L) in
  let io2 = Io.init (Node.id node2) in_ch2 out_ch2 in
  let view2 =
    View.add (Node.init (u64 210L))
      (View.add (Node.init (u64 220L))
         (View.add (Node.init (u64 230L))
            (View.add (Node.init (u64 240L))
               (View.add (Node.init (u64 250L))
                  (View.add (Node.init (u64 260L))
                     (View.add (Node.init (u64 270L))
                        View.empty)))))) in
  let vc2 = Vicinity_lwt.init ~me:node2 ~view:view2 ~view_len ~xchg_len ~period ~io:io2 in

  let timeout = Lwt_unix.sleep 5.5 in
  Lwt_main.run @@
    Lwt.choose [ Vicinity_lwt.run vc1;
                 Vicinity_lwt.run vc2;
                 read_chan in_ch1 vc1 node1 node2;
                 read_chan in_ch2 vc2 node2 node1;
                 Lwt.map (fun () -> Vicinity_lwt.shutdown vc1;
                                    Vicinity_lwt.shutdown vc2) timeout ]
