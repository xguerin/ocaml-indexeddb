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

let opt_string x ~if_missing =
  Js.Optdef.case x (fun () -> if_missing) (fun x -> Js.to_string x)

exception AbortError

let idb_error typ (event : Js_api.request Js_api.errorEvent Js.t) =
  let failure msg =
    Failure (Printf.sprintf "IndexedDB operation (%s) failed: %s" typ msg) in
  Js.Opt.case event##.target
    (fun () -> failure "(missing target on error event)")
    (fun target ->
      Js.Opt.case target##.error
        (fun () -> failure "(missing error on request)")
        (fun error ->
          let name = opt_string error##.name ~if_missing:"(no name)" in
          let message = opt_string error##.message ~if_missing:"(no message)" in
          let code = Js.Optdef.get error##.code (fun () -> 0) in
          if name = "AbortError"
          then AbortError
          else
            failure (Printf.sprintf "%s: %s (error code %d)" name message code)))

let get_factory () =
  let factory : 'a Js_api.factory Js.t Js.Optdef.t =
    (Obj.magic Dom_html.window)##.indexedDB in
  Js.Optdef.get factory (fun () -> failwith "IndexedDB not available")
