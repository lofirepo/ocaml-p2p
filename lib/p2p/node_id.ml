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

open Stdint
open Uint64

type t = Uint64.t

let zero = Uint64.zero
let one = Uint64.one

let random () =
  let open Int64 in
  let r1 = Nocrypto.Rng.Int64.gen max_int in
  let r2 = Nocrypto.Rng.Int64.gen max_int in
  let open Uint64 in
  of_int64 r1 + of_int64 r2

let compare a b =
  if a = b then 0
  else if a < b then -1
  else 1

let distance a b =
  abs @@ a - b

let distance_ring a b =
  let d = abs (a - b) in
  if d = zero
  then (0, d)
  else
    let d = if d <= (max_int - min_int) / (of_int 2)
            then d
            else max_int + one - d in
    if (a - b = d)
    then (-1, d)
    else (1, d)

let to_uint64 t = t

let to_string = Uint64.to_string

let pp ppf t =
  Fmt.pf ppf "%s" @@ Uint64.to_string t
