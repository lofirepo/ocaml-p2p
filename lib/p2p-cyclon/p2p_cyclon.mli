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

{1 CYCLON: Inexpensive Membership Management for Unstructured P2P Overlays}

This is an OCaml implementation of the CYCLON protocol as specified in the paper
{{:https://www.distributed-systems.net/my-data/papers/2005.jnsm.pdf}
CYCLON: Inexpensive Membership Managementfor Unstructured P2P Overlays}.

{2 Protocol}

+ Increase by one the age of all neighbors.
+ Select neighbor {e Q} with the highest age among all neighbors, and {e l - 1} other random neighbors.
+ Replace {e Q}'s entry with a new entry of age 0 and with {e P}'s address.
+ Send the updated subset to peer {e Q}.
+ Receive from {e Q} a subset of no more that {e i} of its own entries.
+ Discard entries pointing at {e P} and entries already contained in {e P}'s cache.
+ Update {e P}'s cache to include {e all} remaining entries, by {e firstly} using empty cache slots (if any),
  and {e secondly} replacing entries among the ones sent to {e Q}.

{e Quoted from the paper above.}

{2 Security}

CYCLON is not secure against malicious nodes deviating from the protocol.
See {{:https://github.com/p2pcollab/ocaml-urps} URPS} for a stream sampler
implementation that achieves uniformity in presence of malicious nodes
and which can be used together with CYCLON.
*)

open P2p

module Make
         (Node_id : S.NODE_ID)
         (Node : S.NODE with type nid := Node_id.t)
         (View : S.VIEW with type nid := Node_id.t
                        and type node := Node.t)
       : S.GOSSIP with type node := Node.t
                   and type view := View.t
