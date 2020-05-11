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
open Js_of_ocaml_lwt
open Lwt
open Utils

type 'a t = 'a Js_api.objectStore Js.t

(*
 * We reuse transactions where possible for performance.
 * This does mean that if any read fails then the others
 * will hang, but we treat any read failing as a fatal error
 * anyway.
 *)

type 'a wrapper =
  { db : 'a Db.t
  ; name : Js.js_string Js.t
  ; mutable ro_trans :
      ('a Js_api.transaction Js.t * (exn -> unit) list ref) option }

type 'a value = 'a Js.t

let wrap db name = {db; name = Js.string name; ro_trans = None}

let create ?keyPath ?(autoIncrement = false) db name =
  let name = Js.string name in
  let keyPath = Option.map (fun e -> Js.string e) keyPath in
  let options =
    object%js
      val mutable keyPath = Js.Optdef.option keyPath

      val mutable autoIncrement = Js.bool autoIncrement
    end in
  db##createObjectStore name options

let delete db name = db##deleteObjectStore (Js.string name)

let rec trans_ro store setup =
  let r, set_r = Lwt.wait () in
  match store.ro_trans with
  | None ->
    let breakers = ref [Lwt.wakeup_exn set_r] in
    let trans =
      store.db##transaction (Js.array [|store.name|]) (Js.string "readonly")
    in
    store.ro_trans <- Some (trans, breakers);
    trans##.onerror :=
      Dom.handler (fun event ->
          store.ro_trans <- None;
          let ex = idb_error "RO" event in
          if ex = AbortError
          then
            print_endline
              "IndexedDB transaction failed (Safari bug?): will wait and retry";
          !breakers |> List.iter (fun b -> b ex);
          Js._true);
    trans##.oncomplete :=
      Dom.handler (fun _event ->
          store.ro_trans <- None;
          Js._true);
    setup (trans##objectStore store.name) set_r;
    r
  | Some (trans, breakers) -> (
    try
      (* Seems we can get here when a transaction is done but oncomplete hasn't been called,
       * so retry if we get an error. *)
      setup (trans##objectStore store.name) set_r;
      breakers := Lwt.wakeup_exn set_r :: !breakers;
      r
    with _ex ->
      store.ro_trans <- None;
      trans_ro store setup)

(*
 * On Safari, transactions can fail unpredictably, so wrap [trans_ro]
 * with auto-retry. See: https://github.com/talex5/cuekeeper/issues/9
 *)

let trans_ro t setup =
  let rec retry delay =
    Lwt.catch
      (fun () -> trans_ro t setup)
      (function
        | AbortError ->
          Lwt_js.sleep (Random.float delay) >>= fun () -> retry (delay *. 1.2)
        | ex -> fail ex) in
  retry 1.0

let trans_rw store setup =
  let r, set_r = Lwt.wait () in
  let trans =
    store.db##transaction (Js.array [|store.name|]) (Js.string "readwrite")
  in
  trans##.onerror :=
    Dom.handler (fun event ->
        Lwt.wakeup_exn set_r (idb_error "RW" event);
        Js._true);
  trans##.onabort :=
    Dom.handler (fun event ->
        Lwt.wakeup_exn set_r (idb_error "RW" event);
        Js._true);
  trans##.oncomplete :=
    Dom.handler (fun _event -> Lwt.wakeup set_r (); Js._true);
  setup (trans##objectStore store.name);
  r

let all t =
  trans_ro t (fun store set_r ->
      let request = store##getAll in
      request##.onsuccess :=
        Dom.handler (fun _event ->
            let result = Js.to_array request##.result in
            Lwt.wakeup set_r result; Js._true))

let count t =
  trans_ro t (fun store set_r ->
      let request = store##count in
      request##.onsuccess :=
        Dom.handler (fun _event ->
            Lwt.wakeup set_r request##.result;
            Js._true))

let set t value = trans_rw t (fun store -> store##put value |> ignore)

let setk t key value =
  trans_rw t (fun store -> store##put_key value (Js.string key) |> ignore)

let setn t values =
  trans_rw t (fun store -> Array.iter (fun e -> store##put e |> ignore) values)

let get t key =
  trans_ro t (fun store set_r ->
      let request = store##get (Js.string key) in
      request##.onsuccess :=
        Dom.handler (fun _event ->
            Js.Optdef.case request##.result (fun () -> None) (fun s -> Some s)
            |> Lwt.wakeup set_r;
            Js._true))

let clear t = trans_rw t (fun store -> store##clear |> ignore)

let compare_and_set t key ~test ~new_value =
  let result = ref None in
  let key = Js.string key in
  trans_rw t (fun store ->
      let request = store##get key in
      request##.onsuccess :=
        Dom.handler (fun _event ->
            if test (Js.Optdef.to_option request##.result)
            then (
              (match new_value with
              | None -> store##delete key |> ignore
              | Some new_value -> store##put_key new_value key |> ignore);
              result := Some true)
            else result := Some false;
            Js._true))
  >|= fun () ->
  match !result with
  | None -> failwith "Transaction completed, but no result!"
  | Some x -> x

let remove t key =
  trans_rw t (fun store -> store##delete (Js.string key) |> ignore)

(*
 * Iterators.
 *)

let fold f acc t =
  let acc = ref acc in
  trans_ro t (fun store set_r ->
      let request = store##openCursor in
      request##.onsuccess :=
        Dom.handler (fun _event ->
            Js.Opt.case request##.result
              (fun () -> Lwt.wakeup set_r !acc)
              (fun cursor ->
                let key = cursor##.key |> Js.to_string
                and value = cursor##.value in
                acc := f !acc key value;
                cursor##continue);
            Js._true))

let filter f t =
  let acc = new%js Js.array_empty in
  trans_ro t (fun store set_r ->
      let request = store##openCursor in
      request##.onsuccess :=
        Dom.handler (fun _event ->
            Js.Opt.case request##.result
              (fun () -> Lwt.wakeup set_r (Js.to_array acc))
              (fun cursor ->
                let key = cursor##.key |> Js.to_string
                and value = cursor##.value in
                if f key then acc##push value |> ignore;
                cursor##continue);
            Js._true))

let range t off count =
  let bindings = new%js Js.array_empty in
  let position = ref 0 in
  trans_ro t (fun store set_r ->
      let request = store##openCursor in
      request##.onsuccess :=
        Dom.handler (fun _event ->
            Js.Opt.case request##.result
              (fun () -> Lwt.wakeup set_r (Js.to_array bindings))
              (fun cursor ->
                if !position < off
                then (
                  position := !position + 1;
                  cursor##continue)
                else if !position < off + count
                then (
                  let value = cursor##.value in
                  bindings##push value |> ignore;
                  position := !position + 1;
                  cursor##continue)
                else Lwt.wakeup set_r (Js.to_array bindings));
            Js._true))

let range_from t key count =
  let bindings = new%js Js.array_empty in
  let position = ref 0 in
  let phase = ref `Init in
  trans_ro t (fun store set_r ->
      let request = store##openCursor in
      request##.onsuccess :=
        Dom.handler (fun _event ->
            Js.Opt.case request##.result
              (fun () -> Lwt.wakeup set_r (Js.to_array bindings))
              (fun cursor ->
                match !phase with
                | `Init ->
                  cursor##continue_key (Js.string key);
                  phase := `Collect
                | `Collect ->
                  if !position < count
                  then (
                    let value = cursor##.value in
                    bindings##push value |> ignore;
                    position := !position + 1;
                    cursor##continue)
                  else Lwt.wakeup set_r (Js.to_array bindings));
            Js._true))

(*
 * Indexes.
 *)

module Index = struct
  type 'a store = 'a t
  type t = Js_api.index Js.t

  (*
   * Creation/deletion.
   *)

  let create store name key =
    store##createIndex (Js.string name) (Js.string key)

  let creatn store name keys =
    let keys = List.map Js.string keys |> Array.of_list |> Js.array in
    store##createIndex_array (Js.string name) keys

  let delete store name = store##deleteIndex (Js.string name)

  (*
   * Data operations.
   *)

  let all name store =
    let name = Js.string name in
    trans_ro store (fun store set_r ->
        let index = store##index name in
        let request = index##getAll in
        request##.onsuccess :=
          Dom.handler (fun _event ->
              let result = Js.to_array request##.result in
              Lwt.wakeup set_r result; Js._true))

  let count name store =
    let name = Js.string name in
    trans_ro store (fun store set_r ->
        let index = store##index name in
        let request = index##count in
        request##.onsuccess :=
          Dom.handler (fun _event ->
              Lwt.wakeup set_r request##.result;
              Js._true))

  (*
   * Iterators.
   *)

  let filter name f t =
    let name = Js.string name in
    let acc = new%js Js.array_empty in
    trans_ro t (fun store set_r ->
        let index = store##index name in
        let request = index##openCursor in
        request##.onsuccess :=
          Dom.handler (fun _event ->
              Js.Opt.case request##.result
                (fun () -> Lwt.wakeup set_r (Js.to_array acc))
                (fun cursor ->
                  let key = cursor##.primaryKey |> Js.to_string
                  and value = cursor##.value in
                  if f key then acc##push value |> ignore;
                  cursor##continue);
              Js._true))

  let range name t off count =
    let name = Js.string name in
    let bindings = new%js Js.array_empty in
    let position = ref 0 in
    trans_ro t (fun store set_r ->
        let index = store##index name in
        let request = index##openCursor in
        request##.onsuccess :=
          Dom.handler (fun _event ->
              Js.Opt.case request##.result
                (fun () -> Lwt.wakeup set_r (Js.to_array bindings))
                (fun cursor ->
                  if !position < off
                  then (
                    position := !position + 1;
                    cursor##continue)
                  else if !position < off + count
                  then (
                    let value = cursor##.value in
                    bindings##push value |> ignore;
                    position := !position + 1;
                    cursor##continue)
                  else Lwt.wakeup set_r (Js.to_array bindings));
              Js._true))
end
