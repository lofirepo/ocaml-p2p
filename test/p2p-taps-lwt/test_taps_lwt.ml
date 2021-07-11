(*
  Copyright (C) 2021 TG x Thoth

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

module Taps_config : P2p_taps.S.CONFIG = struct
  let min_trust = 0.1
  let trust_transitivity = 0.9
end

module Node_id = P2p.Node_id
module Node = P2p.Node.Make (Node_id)
module View = P2p.View.Make (Node_id) (Node)
module Taps = P2p_taps.Make (Node_id) (Node) (View) (Taps_config)

module Io = struct

  type t = {
      node_id : Node_id.t;
      xview : View.t;
      in_chan : Lwt_io.input_channel;
      out_chan : Lwt_io.output_channel;
    }

  let init node_id xview in_chan out_chan =
    { node_id; xview; in_chan; out_chan }

  (** [initiate_gossip t node xchg]
      sends [xchg] entries to node [dst]
      and returns response *)
  let initiate_gossip t dst xchg =
    pf out "%a # INITIATE_GOSSIP to node %a\n" Node_id.pp t.node_id Node.pp dst;
    pf out "%a # xchg to send:\n%a\n" Node_id.pp t.node_id View.pp xchg; flush stdout;
    flush stdout;
    let%lwt _ = Lwt_io.write_value t.out_chan xchg in
    Lwt_io.read_value t.in_chan

  (** [respond_gossip t node xchg]
      sends [xchg] entries in response to node [dst] *)
  let respond_gossip t dst xchg =
    pf out "%a # RESPOND_GOSSIP to node %a\n" Node_id.pp t.node_id Node.pp dst;
    pf out "%a # xchg to send:\n%a\n" Node_id.pp t.node_id View.pp xchg;
    flush stdout;
    let%lwt _ = Lwt_io.write_value t.out_chan xchg in
    Lwt.return_unit

  (** [gossip_recvd t view src recvd]
      is called after entries are received from node [src] during a gossip exchange *)
  let gossip_recvd _t _src recvd _view =
    Lwt.return recvd

  (** [view_updated node view]
      is called when [view] has been updated after a gossip exchange *)
  let view_updated t node view =
    pf out "%a # VIEW_UPDATED of node %a\n%a\n" Node_id.pp t.node_id Node.pp node View.pp view;
    flush stdout;
    Lwt.return_unit

  let get_xview t =
    t.xview
end

module Taps_lwt = P2p_taps_lwt.Make (Node_id) (Node) (View) (Taps) (Io)

let rec read_chan ch cy node rnode =
  let%lwt recvd = Lwt_io.read_value ch in
  let nid = Node.id node in
  pf out "%a # READ_CHAN\n" Node_id.pp nid;
  pf out "%a # recvd:\n%a\n" Node_id.pp nid View.pp recvd;
  flush stdout;
  let%lwt view = Taps_lwt.respond cy rnode recvd in
  pf out "%a # recvd:\n%a\n" Node_id.pp nid View.pp recvd;
  pf out "%a # view:\n%a\n" Node_id.pp nid View.pp view;
  flush stdout;
  read_chan ch cy node rnode

let _ = Nocrypto_entropy_lwt.initialize ()

let () =
  let view_len = 8 in
  let xchg_len = 4 in
  let period = 0.1 in

  let (in_ch1, out_ch2) = Lwt_io.pipe () in
  let (in_ch2, out_ch1) = Lwt_io.pipe () in

  let node1 = Node.init (u64 100L) ~trust:1.0 ~known:true in
  let view1 =
    View.add (Node.init (u64 200L) ~trust:0.75 ~known:true)
      (View.add (Node.init (u64 110L) ~trust:0.9 ~known:true)
         (View.add (Node.init (u64 120L) ~trust:0.8 ~known:true)
            (View.add (Node.init (u64 130L) ~trust:0.7 ~known:true)
               (View.add (Node.init (u64 140L) ~trust:0.6 ~known:false)
                  (View.add (Node.init (u64 150L) ~trust:0.5 ~known:false)
                     (View.add (Node.init (u64 160L) ~trust:0.4 ~known:false)
                        (View.add (Node.init (u64 170L) ~trust:0.3 ~known:false)
                           View.empty))))))) in
  let io1 = Io.init (Node.id node1) view1 in_ch1 out_ch1 in
  let taps1 = Taps_lwt.init ~me:node1 ~view:View.empty ~view_len ~xchg_len ~period ~io:io1 in

  let node2 = Node.init (u64 200L) ~trust:1.0 ~known:true in
  let view2 =
    View.add (Node.init (u64 100L) ~trust:0.5 ~known:true)
      (View.add (Node.init (u64 190L) ~trust:0.9 ~known:true)
         (View.add (Node.init (u64 180L) ~trust:0.8 ~known:true)
            (View.add (Node.init (u64 170L) ~trust:0.7 ~known:true)
               (View.add (Node.init (u64 160L) ~trust:0.6 ~known:false)
                  (View.add (Node.init (u64 210L) ~trust:0.5 ~known:false)
                     (View.add (Node.init (u64 220L) ~trust:0.4 ~known:false)
                        (View.add (Node.init (u64 230L) ~trust:0.3 ~known:false)
                           View.empty))))))) in
  let io2 = Io.init (Node.id node2) view2 in_ch2 out_ch2 in
  let taps2 = Taps_lwt.init ~me:node2 ~view:View.empty ~view_len ~xchg_len ~period ~io:io2 in

  let timeout = Lwt_unix.sleep 5.55 in
  Lwt_main.run @@
    Lwt.choose [ Taps_lwt.run taps1;
                 Taps_lwt.run taps2;
                 read_chan in_ch1 taps1 node1 node2;
                 read_chan in_ch2 taps2 node2 node1;
                 Lwt.map (fun () -> Taps_lwt.shutdown taps1;
                                    Taps_lwt.shutdown taps2) timeout ]
