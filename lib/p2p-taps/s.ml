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

(** TAPS protocol configuration parameters *)
module type CONFIG = sig

  (** trust transitivity factor to reduce each received node's trust value with
      (in addition to the source node's trust value) *)
  val trust_transitivity : float

  (** minimum trust value for a node to be considered for merging into view *)
  val min_trust : float

end
