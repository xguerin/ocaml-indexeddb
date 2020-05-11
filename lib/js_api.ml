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

(**
 * JSOO type declarations for the w3c IndexedDB.
 * http://www.w3.org/TR/IndexedDB/
 *)

open Js_of_ocaml

(*
 * Interface.
 *)

type key = Js.js_string Js.t
type index_name = Js.js_string Js.t
type store_name = Js.js_string Js.t
type mode = Js.js_string Js.t

class type versionChangeEvent =
  object
    inherit Dom_html.event

    method oldVersion : int Js.readonly_prop

    method newVersion : int Js.readonly_prop Js.Opt.t
  end

class type ['a] errorEvent =
  object
    inherit ['a] Dom.event
  end

class type completeEvent =
  object
    inherit Dom_html.event
  end

class type successEvent =
  object
    inherit Dom_html.event
  end

class type cursor =
  object
    method key : key Js.readonly_prop

    method primaryKey : key Js.readonly_prop

    method continue : unit Js.meth

    method continue_key : key -> unit Js.meth
  end

class type ['a] cursorWithValue =
  object
    inherit cursor

    method value : 'a Js.t Js.readonly_prop
  end

class type dom_exception =
  object
    (* Being a bit paranoid marking all these as optdef *)
    method name : Js.js_string Js.t Js.Optdef.t Js.readonly_prop

    method message : Js.js_string Js.t Js.Optdef.t Js.readonly_prop

    method code : int Js.Optdef.t Js.readonly_prop
  end

class type request =
  object
    method error : dom_exception Js.t Js.Opt.t Js.readonly_prop

    method onerror :
      ('self Js.t, request errorEvent Js.t) Dom.event_listener Js.prop

    method onsuccess :
      ('self Js.t, successEvent Js.t) Dom.event_listener Js.prop
  end

class type countRequest =
  object ('self)
    inherit request

    method result : int Js.readonly_prop
  end

class type ['a] getRequest =
  object ('self)
    inherit request

    method result : 'a Js.Optdef.t Js.readonly_prop
  end

class type ['a] getAllRequest =
  object ('self)
    inherit request

    method result : 'a Js.js_array Js.t Js.readonly_prop
  end

class type ['a] openCursorRequest =
  object
    inherit request

    method result : 'a cursorWithValue Js.t Js.Opt.t Js.readonly_prop
  end

class type index =
  object
    method count : countRequest Js.t Js.meth

    method getAll : 'a Js.t getAllRequest Js.t Js.meth

    method openCursor : 'a openCursorRequest Js.t Js.meth
  end

class type objectStoreOptions =
  object
    method keyPath : Js.js_string Js.t Js.Optdef.t Js.prop

    method autoIncrement : bool Js.t Js.prop
  end

class type ['a] objectStore =
  object
    method add : 'a Js.t -> request Js.t Js.meth

    method add_key : 'a Js.t -> key -> request Js.t Js.meth

    method count : countRequest Js.t Js.meth

    method put : 'a Js.t -> request Js.t Js.meth

    method put_key : 'a Js.t -> key -> request Js.t Js.meth

    method delete : key -> request Js.t Js.meth

    method get : key -> 'a Js.t getRequest Js.t Js.meth

    method getAll : 'a Js.t getAllRequest Js.t Js.meth

    method clear : request Js.t Js.meth

    method openCursor : 'a openCursorRequest Js.t Js.meth

    method createIndex : index_name -> key -> index Js.t Js.meth

    method createIndex_array :
      index_name -> key Js.js_array Js.t -> index Js.t Js.meth

    method deleteIndex : index_name -> unit Js.meth

    method index : index_name -> index Js.t Js.meth
  end

class type ['a] transaction =
  object
    method oncomplete :
      ('self Js.t, completeEvent Js.t) Dom.event_listener Js.prop

    method onerror :
      ('self Js.t, request errorEvent Js.t) Dom.event_listener Js.prop

    method onabort :
      ('self Js.t, request errorEvent Js.t) Dom.event_listener Js.prop

    method objectStore : store_name -> 'a objectStore Js.t Js.meth

    method abort : unit Js.meth
  end

class type ['a] database =
  object
    method close : unit Js.meth

    method createObjectStore :
      store_name -> objectStoreOptions Js.t -> 'a objectStore Js.t Js.meth

    method deleteObjectStore : store_name -> unit Js.meth

    method onerror :
      ('self Js.t, request errorEvent Js.t) Dom.event_listener Js.prop

    method transaction :
      store_name Js.js_array Js.t -> mode -> 'a transaction Js.t Js.meth
  end

class type ['a] openDBRequest =
  object ('self)
    inherit request

    method onupgradeneeded :
      ('self Js.t, versionChangeEvent Js.t) Dom_html.event_listener Js.prop

    method onblocked :
      ('self Js.t, versionChangeEvent Js.t) Dom_html.event_listener Js.prop

    method result : 'a database Js.t Js.readonly_prop
  end

class type ['a] factory =
  object
    method _open : Js.js_string Js.t -> int -> 'a openDBRequest Js.t Js.meth

    method deleteDatabase : Js.js_string Js.t -> 'a openDBRequest Js.t Js.meth
  end
