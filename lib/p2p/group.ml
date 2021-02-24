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

module Make
         (Group_id : S.GROUP_ID)
       : S.GROUP with type gid := Group_id.t = struct

  type gid = Group_id.t

  type t = {
      id : gid;
    }

  let init id = { id }

  let id t = t.id

  let compare a b =
    Group_id.compare a.id b.id

  let pp ppf t =
    Fmt.pf ppf "%a" Group_id.pp t.id

end
