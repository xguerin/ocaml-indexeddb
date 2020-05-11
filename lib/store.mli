(*
 * Copyright (c) 2019 Thomas Leonard <talex5@gmail.com>
 * Copyright (c) 2020 Xavier R. Guérin <copyright@applepine.org>
 * 
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Js_of_ocaml

type 'a t
type 'a wrapper
type 'a value = 'a Js.t

(*
 * Creation/deletion.
 *)

val wrap : 'a Db.t -> string -> 'a wrapper
val create : ?keyPath:string -> ?autoIncrement:bool -> 'a Db.t -> string -> 'a t
val delete : 'a Db.t -> string -> unit

(*
 * Data operation.
 *)

val all : 'a wrapper -> 'a value array Lwt.t
val count : 'a wrapper -> int Lwt.t
val set : 'a wrapper -> 'a value -> unit Lwt.t
val setk : 'a wrapper -> string -> 'a value -> unit Lwt.t
val setn : 'a wrapper -> 'a value array -> unit Lwt.t
val get : 'a wrapper -> string -> 'a value option Lwt.t
val clear : 'a wrapper -> unit Lwt.t

val compare_and_set :
     'a wrapper
  -> string
  -> test:('a value option -> bool)
  -> new_value:'a value option
  -> bool Lwt.t

val remove : 'a wrapper -> string -> unit Lwt.t

(*
 * Iterators.
 *)

val fold :
  ('acc -> string -> 'a value -> 'acc) -> 'acc -> 'a wrapper -> 'acc Lwt.t

val filter : (string -> bool) -> 'a wrapper -> 'a value array Lwt.t
val range : 'a wrapper -> int -> int -> 'a value array Lwt.t
val range_from : 'a wrapper -> string -> int -> 'a value array Lwt.t

(*
 * Index.
 *)

module Index : sig
  type 'a store = 'a t
  type t

  (*
   * Creation/deletion.
   *)

  val create : 'a store -> string -> string -> t
  val creatn : 'a store -> string -> string list -> t
  val delete : 'a store -> string -> unit

  (*
   * Data operations.
   *)

  val all : string -> 'a wrapper -> 'a value array Lwt.t
  val count : string -> 'a wrapper -> int Lwt.t

  (*
   * Iterators.
   *)

  val filter : string -> (string -> bool) -> 'a wrapper -> 'a value array Lwt.t
  val range : string -> 'a wrapper -> int -> int -> 'a value array Lwt.t
end
