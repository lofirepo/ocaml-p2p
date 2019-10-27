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

module Make
         (Node_id : S.NODE_ID)
       : S.NODE with type nid := Node_id.t = struct

  type nid = Node_id.t

  type t = {
      id : nid;
      age : int;
      ver : int;
    }

  let id t = t.id
  let age t = t.age
  let ver t = t.ver

  let init ?(age=0) ?(ver=0) id =
    { id; age; ver }

  let compare a b =
    Node_id.compare a.id b.id

  let distance a b =
    Node_id.distance a.id b.id

  let distance_ring a b =
    Node_id.distance_ring a.id b.id

  let zero_age t =
    { t with age = 0 }

  let incr_age t =
    { t with age = t.age + 1 }

  let set_age t age =
    { t with age }

  let incr_ver t =
    { t with ver = t.ver + 1 }

  let set_ver t ver =
    { t with ver }

  let to_string t =
    Printf.sprintf "%s (ver: %d; age: %d)"
      (Node_id.to_string t.id) t.ver t.age

  let pp ppf t =
    Fmt.pf ppf "%s (ver: %d; age: %d)\n"
      (Node_id.to_string t.id) t.ver t.age

end
