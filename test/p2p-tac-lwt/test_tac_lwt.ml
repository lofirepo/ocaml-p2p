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

module Tac_config : P2p_tac.S.CONFIG = struct
  let trust_weight = 1.
  let min_trust = 0.1
end

module Node_id = P2p.Node_id
module Node = P2p.Node.Make (Node_id)
module View = P2p.View.Make (Node_id) (Node)
module Tac = P2p_tac.Make (Node_id) (Node) (View) (Tac_config)

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

module Tac_lwt = P2p_tac_lwt.Make (Node_id) (Node) (View) (Tac) (Io)

let rec read_chan ch cy node rnode =
  let%lwt recvd = Lwt_io.read_value ch in
  let nid = Node.id node in
  pf out "%a # READ_CHAN\n" Node_id.pp nid;
  pf out "%a # recvd:\n%a\n" Node_id.pp nid View.pp recvd;
  flush stdout;
  let%lwt view = Tac_lwt.respond cy rnode recvd in
  pf out "%a # recvd:\n%a\n" Node_id.pp nid View.pp recvd;
  pf out "%a # view:\n%a\n" Node_id.pp nid View.pp view;
  flush stdout;
  read_chan ch cy node rnode

let do_blip = false

let blip bf =
  let bits = Bloomf.bits bf in
  let e = 13. in
  let _m, k = Bloomf.params bf in
  let p = Blip.p e k in
  Blip.flip bits p

let subs subsl =
  let bf = Bloomf.create 10 in
  List.iter (fun sub -> Bloomf.add bf sub) subsl;
  if do_blip
  then blip bf
  else Bloomf.bits bf

let _ = Nocrypto_entropy_lwt.initialize ()

let () =
  let view_len = 8 in
  let xchg_len = 4 in
  let period = 0.1 in

  let (in_ch1, out_ch2) = Lwt_io.pipe () in
  let (in_ch2, out_ch1) = Lwt_io.pipe () in

  let node1 = Node.init (u64 100L) ~trust:1.0 ~known:true ~sim:1.0 ~subs:(subs ["A"; "B"; "C"]) in
  let xview1 =
    View.add (Node.init (u64 7L) ~trust:0.9 ~subs:(subs ["A"]) ~known:true)
      (View.add (Node.init (u64 11L) ~trust:0.8 ~subs:(subs ["A"; "B"]) ~known:true)
         (View.add (Node.init (u64 13L) ~trust:0.7 ~subs:(subs ["D"]) ~known:true)
            (View.add (Node.init (u64 17L) ~trust:0.6 ~subs:(subs ["B"]) ~known:true)
               (View.add (Node.init (u64 19L) ~trust:0.5 ~subs:(subs ["C"]))
                  (View.add (Node.init (u64 29L) ~trust:0.4 ~subs:(subs ["B"; "C"]))
                     (View.add (Node.init (u64 37L) ~trust:0.3 ~subs:(subs ["A"; "B"; "C"]))
                        View.empty)))))) in
  let view1 =
    View.add (Node.init (u64 10L) ~trust:0.9 ~subs:(subs ["A"]))
      (View.add (Node.init (u64 20L) ~trust:0.8 ~subs:(subs ["A"; "B"]))
         (View.add (Node.init (u64 30L) ~trust:0.7 ~subs:(subs ["D"]) ~age:1)
            (View.add (Node.init (u64 40L) ~trust:0.6 ~subs:(subs ["B"; "D"]))
               (View.add (Node.init (u64 50L) ~trust:0.5 ~subs:(subs ["C"]))
                  (View.add (Node.init (u64 60L) ~trust:0.4 ~subs:(subs ["B"; "C"]))
                     (View.add (Node.init (u64 70L) ~trust:0.3 ~subs:(subs ["A"; "B"; "C"]))
                        View.empty)))))) in
  let io1 = Io.init (Node.id node1) xview1 in_ch1 out_ch1 in
  let tac1 = Tac_lwt.init ~me:node1 ~view:view1 ~view_len ~xchg_len ~period ~io:io1 in

  let node2 = Node.init (u64 200L) ~trust:1.0 ~known:true ~sim:1.0 ~subs:(subs ["C"; "D"; "E"]) in
  let xview2 =
    View.add (Node.init (u64 59L) ~trust:0.6 ~subs:(subs ["A"; "B"; "C"]))
      (View.add (Node.init (u64 69L) ~trust:0.7 ~subs:(subs ["A"; "B"]))
         (View.add (Node.init (u64 79L) ~trust:0.8 ~subs:(subs ["B"; "C"]) ~known:true)
            (View.add (Node.init (u64 89L) ~trust:0.9 ~subs:(subs ["C"]) ~known:true)
               (View.add (Node.init (u64 99L) ~trust:1.0 ~subs:(subs ["A"; "B"; "C"]) ~known:true)
                  View.empty)))) in
  let view2 =
    View.add (Node.init (u64 10L) ~trust:0.9 ~subs:(subs ["A"]))
      (View.add (Node.init (u64 20L) ~trust:0.8 ~subs:(subs ["A"; "B"]))
         (View.add (Node.init (u64 30L) ~trust:0.7 ~subs:(subs ["D"]))
            (View.add (Node.init (u64 40L) ~trust:0.6 ~subs:(subs ["B"; "D"]))
               (View.add (Node.init (u64 50L) ~trust:0.5 ~subs:(subs ["C"]))
                  (View.add (Node.init (u64 60L) ~trust:0.4 ~subs:(subs ["B"; "C"]))
                     (View.add (Node.init (u64 70L) ~trust:0.3 ~subs:(subs ["A"; "B"; "C"]))
                        View.empty)))))) in
  let io2 = Io.init (Node.id node2) xview2 in_ch2 out_ch2 in
  let tac2 = Tac_lwt.init ~me:node2 ~view:view2 ~view_len ~xchg_len ~period ~io:io2 in

  let timeout = Lwt_unix.sleep 5.55 in
  Lwt_main.run @@
    Lwt.choose [ Tac_lwt.run tac1;
                 Tac_lwt.run tac2;
                 read_chan in_ch1 tac1 node1 node2;
                 read_chan in_ch2 tac2 node2 node1;
                 Lwt.map (fun () -> Tac_lwt.shutdown tac1;
                                    Tac_lwt.shutdown tac2) timeout ]
