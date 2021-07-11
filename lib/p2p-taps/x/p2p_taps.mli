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

{1 TAPS: Trust-Aware Peer Sampling}

This is an OCaml implementation of the Trust-Aware Peer Sampling (TAPS) protocol described in the thesis
{{:https://tel.archives-ouvertes.fr/tel-01135867/file/JEGOU_Arnaud.pdf}
Harnessing the power of implicit and explicit socialnetworks through decentralization}.

{2 Protocol}

The protocol is based on a gossip exchange,
similar to random peer sampling protocols such as CYCLON,
but biases node selection based on local trust information.

This implementation differs from TAPS2 in two key ways:
1. trust values are asymmetric,
   thus no agreement necessary between nodes about symmetric trust values
2. nodes only use local information to establish trust values,
   without relying on multi-hop trust paths.
   They store trust information about direct friends,
   and discover friends-of-friends via a private set intersection protocol

Each node maintains two views:
+ an EXPLICIT view of nodes with associated local asymmetric trust values,
+ a dynamic TAPS view that view exchanges can modify

The view exchange mechanism this module implements is identical to the one from TAPS2,
with the following properties:

+ Biased view selection: prefer more trusted entries when merging received views
+ Continuous bootsrapping: continuously exchange information between both views of both types of nodes,
  thus TAPS-TAPS, TAPS-EXPL, EXPL-TAPS, EXPL-EXPL exchanges all happen in a randomized manner

{2 Extensions}

*)

open P2p

module Make
         (Node_id : S.NODE_ID)
         (Node : S.NODE with type nid := Node_id.t)
         (View : S.VIEW with type nid := Node_id.t
                         and type node := Node.t)
       : S.GOSSIP with type node := Node.t
                   and type view := View.t
