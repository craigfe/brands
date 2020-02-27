(*
 * Copyright (c) 2020 Craig Ferguson <me@craigfe.io>
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

open Brands_intf

type ('a, 'fn) app = ..

type ('b, 'a, 'fn) app2 = ('b, ('a, 'fn) app) app

type ('c, 'b, 'a, 'fn) app3 = ('c, ('b, 'a, 'fn) app2) app

type ('d, 'c, 'b, 'a, 'fn) app4 = ('d, ('c, 'b, 'a, 'fn) app3) app

type ('e, 'd, 'c, 'b, 'a, 'fn) app5 = ('e, ('d, 'c, 'b, 'a, 'fn) app4) app

type ('f, 'e, 'd, 'c, 'b, 'a, 'fn) app6 =
  ('f, ('e, 'd, 'c, 'b, 'a, 'fn) app5) app

type ('g, 'f, 'e, 'd, 'c, 'b, 'a, 'fn) app7 =
  ('g, ('f, 'e, 'd, 'c, 'b, 'a, 'fn) app6) app

type ('h, 'g, 'f, 'e, 'd, 'c, 'b, 'a, 'fn) app8 =
  ('h, ('g, 'f, 'e, 'd, 'c, 'b, 'a, 'fn) app7) app

module type S1 = sig
  type 'a t

  type br

  val inj : 'a t -> ('a, br) app

  val prj : ('a, br) app -> 'a t
end

module type S2 = sig
  type ('a, 'b) t

  type br

  val inj : ('a, 'b) t -> ('a, 'b, br) app2

  val prj : ('a, 'b, br) app2 -> ('a, 'b) t
end

module type S3 = sig
  type ('a, 'b, 'c) t

  type br

  val inj : ('a, 'b, 'c) t -> ('a, 'b, 'c, br) app3

  val prj : ('a, 'b, 'c, br) app3 -> ('a, 'b, 'c) t
end

module type S4 = sig
  type ('a, 'b, 'c, 'd) t

  type br

  val inj : ('a, 'b, 'c, 'd) t -> ('a, 'b, 'c, 'd, br) app4

  val prj : ('a, 'b, 'c, 'd, br) app4 -> ('a, 'b, 'c, 'd) t
end

module type S5 = sig
  type ('a, 'b, 'c, 'd, 'e) t

  type br

  val inj : ('a, 'b, 'c, 'd, 'e) t -> ('a, 'b, 'c, 'd, 'e, br) app5

  val prj : ('a, 'b, 'c, 'd, 'e, br) app5 -> ('a, 'b, 'c, 'd, 'e) t
end

module type S6 = sig
  type ('a, 'b, 'c, 'd, 'e, 'f) t

  type br

  val inj : ('a, 'b, 'c, 'd, 'e, 'f) t -> ('a, 'b, 'c, 'd, 'e, 'f, br) app6

  val prj : ('a, 'b, 'c, 'd, 'e, 'f, br) app6 -> ('a, 'b, 'c, 'd, 'e, 'f) t
end

module type S7 = sig
  type ('a, 'b, 'c, 'd, 'e, 'f, 'g) t

  type br

  val inj :
    ('a, 'b, 'c, 'd, 'e, 'f, 'g) t -> ('a, 'b, 'c, 'd, 'e, 'f, 'g, br) app7

  val prj :
    ('a, 'b, 'c, 'd, 'e, 'f, 'g, br) app7 -> ('a, 'b, 'c, 'd, 'e, 'f, 'g) t
end

module type S8 = sig
  type ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h) t

  type br

  val inj :
    ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h) t ->
    ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, br) app8

  val prj :
    ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, br) app8 ->
    ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h) t
end

module Make_brand1 (X : T1) () = struct
  type 'a t = 'a X.t

  type br

  type (_, _) app += App : 'a t -> ('a, br) app [@@unboxed]

  let inj v = App v

  let prj = function App v -> v | _ -> assert false
end

module Make_brand2 (X : T2) () = struct
  type ('a, 'b) t = ('a, 'b) X.t

  type br

  type (_, _) app += App : ('a, 'b) t -> ('a, ('b, br) app) app [@@unboxed]

  let inj v = App v

  let prj = function App v -> v | _ -> assert false
end

module Make_brand3 (X : T3) () = struct
  type ('a, 'b, 'c) t = ('a, 'b, 'c) X.t

  type br

  type (_, _) app += App : ('a, 'b, 'c) t -> ('a, ('b, ('c, br) app) app) app
    [@@unboxed]

  let inj v = App v

  let prj = function App v -> v | _ -> assert false
end

module Make_brand4 (X : T4) () = struct
  type ('a, 'b, 'c, 'd) t = ('a, 'b, 'c, 'd) X.t

  type br

  type (_, _) app +=
    | App : ('a, 'b, 'c, 'd) t -> ('a, ('b, ('c, ('d, br) app) app) app) app
    [@@unboxed]

  let inj v = App v

  let prj = function App v -> v | _ -> assert false
end

module Make_brand5 (X : T5) () = struct
  type ('a, 'b, 'c, 'd, 'e) t = ('a, 'b, 'c, 'd, 'e) X.t

  type br

  type (_, _) app +=
    | App :
        ('a, 'b, 'c, 'd, 'e) t
        -> ('a, ('b, ('c, ('d, ('e, br) app) app) app) app) app
    [@@unboxed]

  let inj v = App v

  let prj = function App v -> v | _ -> assert false
end

module Make_brand6 (X : T6) () = struct
  type ('a, 'b, 'c, 'd, 'e, 'f) t = ('a, 'b, 'c, 'd, 'e, 'f) X.t

  type br

  type (_, _) app +=
    | App :
        ('a, 'b, 'c, 'd, 'e, 'f) t
        -> ('a, ('b, ('c, ('d, ('e, ('f, br) app) app) app) app) app) app
    [@@unboxed]

  let inj v = App v

  let prj = function App v -> v | _ -> assert false
end

module Make_brand7 (X : T7) () = struct
  type ('a, 'b, 'c, 'd, 'e, 'f, 'g) t = ('a, 'b, 'c, 'd, 'e, 'f, 'g) X.t

  type br

  type (_, _) app +=
    | App :
        ('a, 'b, 'c, 'd, 'e, 'f, 'g) t
        -> ( 'a,
             ('b, ('c, ('d, ('e, ('f, ('g, br) app) app) app) app) app) app )
           app
    [@@unboxed]

  let inj v = App v

  let prj = function App v -> v | _ -> assert false
end

module Make_brand8 (X : T8) () = struct
  type ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h) t = ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h) X.t

  type br

  type (_, _) app +=
    | App :
        ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h) t
        -> ( 'a,
             ( 'b,
               ('c, ('d, ('e, ('f, ('g, ('h, br) app) app) app) app) app) app
             )
             app )
           app
    [@@unboxed]

  let inj v = App v

  let prj = function App v -> v | _ -> assert false
end

(* Standard library brands *)

module Branded = struct
  module Array = Make_brand1 (Stdlib.Array) ()

  module ArrayLabels = Make_brand1 (Stdlib.ArrayLabels) ()

  module Hashtbl = Make_brand2 (Stdlib.Hashtbl) ()

  module Lazy = Make_brand1 (Stdlib.Lazy) ()

  module List = Make_brand1 (Stdlib.List) ()

  module ListLabels = Make_brand1 (Stdlib.ListLabels) ()

  module Option = Make_brand1 (Stdlib.Option) ()

  module Queue = Make_brand1 (Stdlib.Queue) ()

  module Result = Make_brand2 (Stdlib.Result) ()

  module Seq = Make_brand1 (Stdlib.Seq) ()

  module Stack = Make_brand1 (Stdlib.Stack) ()

  module Stream = Make_brand1 (Stdlib.Stream) ()

  module Weak = Make_brand1 (Stdlib.Weak) ()
end
