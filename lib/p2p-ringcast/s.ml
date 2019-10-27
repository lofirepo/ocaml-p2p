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

module type DISSEMINATION = sig
  type nid
  type node
  type view
  type mid
  type seen

  val init_seen : int -> seen

  val forward :
    view : view
    -> seen : seen
    -> mid : mid
    -> src : node
    -> me : node
    -> fanout : int
    -> (view * seen)
end

module type GOSSIP_DISSEM = sig
  type nid
  type node
  type view
  type mid
  type seen

  include P2p.S.GOSSIP
          with type node := node
           and type view := view

  include DISSEMINATION
          with type nid := nid
           and type node := node
           and type view := view
           and type mid := mid
           and type seen := seen
end
