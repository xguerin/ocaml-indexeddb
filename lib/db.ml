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
open Lwt
open Utils

type 'a t = 'a Js_api.database Js.t

let make name ~version ~init =
  let name = Js.string name in
  let factory = get_factory () in
  let request = factory##_open name version in
  let t, set_t = Lwt.wait () in
  request##.onblocked :=
    Dom.handler (fun _event ->
        print_endline
          "Waiting for other IndexedDB users to close their connections before \
           upgrading schema version.";
        Js._true);
  request##.onupgradeneeded :=
    Dom.handler (fun event ->
        try
          let old_version = event##.oldVersion in
          init ~old_version request##.result;
          Js._true
        with ex ->
          (*
           * Firefox throws the exception away and returns
           * AbortError instead, so save it here.
           *)
          Lwt.wakeup_exn set_t ex; raise ex);
  request##.onerror :=
    Dom.handler (fun event ->
        (match (Lwt.state t, idb_error "open" event) with
        | Fail _, AbortError -> () (* Already reported a better exception *)
        | _, ex -> Lwt.wakeup_exn set_t ex);
        Js._true);
  request##.onsuccess :=
    Dom.handler (fun _event ->
        Lwt.wakeup set_t request##.result;
        Js._true);
  t

let close db = db##close

let delete name =
  let name = Js.string name in
  let factory = get_factory () in
  let request = factory##deleteDatabase name in
  let t, set_t = Lwt.wait () in
  request##.onerror :=
    Dom.handler (fun _event ->
        Lwt.wakeup_exn set_t
          (Failure "Error trying to delete IndexedDB database");
        Js._true);
  request##.onsuccess :=
    Dom.handler (fun _event -> Lwt.wakeup set_t (); Js._true);
  t
