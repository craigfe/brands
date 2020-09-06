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

type ('a, 'fn) app

include Make_interfaces (struct
  type ('a, 'fn) t = ('a, 'fn) app
end)

module Make0 (X : T0) = struct
  type t = X.t
  type br

  external inj : t -> (_, br) app = "%identity"
  external prj : (_, br) app -> t = "%identity"
end

module Make1 (X : T1) = struct
  type 'a t = 'a X.t
  type br

  external inj : 'a t -> ('a, br) app = "%identity"
  external prj : ('a, br) app -> 'a t = "%identity"
end

module Make2 (X : T2) = struct
  type ('a, 'b) t = ('a, 'b) X.t
  type br

  external inj : ('a, 'b) t -> ('a, 'b, br) app2 = "%identity"
  external prj : ('a, 'b, br) app2 -> ('a, 'b) t = "%identity"
end

module Make3 (X : T3) = struct
  type ('a, 'b, 'c) t = ('a, 'b, 'c) X.t
  type br

  external inj : ('a, 'b, 'c) t -> ('a, 'b, 'c, br) app3 = "%identity"
  external prj : ('a, 'b, 'c, br) app3 -> ('a, 'b, 'c) t = "%identity"
end

module Make4 (X : T4) = struct
  type ('a, 'b, 'c, 'd) t = ('a, 'b, 'c, 'd) X.t
  type br

  external inj : ('a, 'b, 'c, 'd) t -> ('a, 'b, 'c, 'd, br) app4 = "%identity"
  external prj : ('a, 'b, 'c, 'd, br) app4 -> ('a, 'b, 'c, 'd) t = "%identity"
end

module Make5 (X : T5) = struct
  type ('a, 'b, 'c, 'd, 'e) t = ('a, 'b, 'c, 'd, 'e) X.t
  type br

  external inj : ('a, 'b, 'c, 'd, 'e) t -> ('a, 'b, 'c, 'd, 'e, br) app5
    = "%identity"

  external prj : ('a, 'b, 'c, 'd, 'e, br) app5 -> ('a, 'b, 'c, 'd, 'e) t
    = "%identity"
end

module Make6 (X : T6) = struct
  type ('a, 'b, 'c, 'd, 'e, 'f) t = ('a, 'b, 'c, 'd, 'e, 'f) X.t
  type br

  external inj : ('a, 'b, 'c, 'd, 'e, 'f) t -> ('a, 'b, 'c, 'd, 'e, 'f, br) app6
    = "%identity"

  external prj : ('a, 'b, 'c, 'd, 'e, 'f, br) app6 -> ('a, 'b, 'c, 'd, 'e, 'f) t
    = "%identity"
end

module Make7 (X : T7) = struct
  type ('a, 'b, 'c, 'd, 'e, 'f, 'g) t = ('a, 'b, 'c, 'd, 'e, 'f, 'g) X.t
  type br

  external inj :
    ('a, 'b, 'c, 'd, 'e, 'f, 'g) t -> ('a, 'b, 'c, 'd, 'e, 'f, 'g, br) app7
    = "%identity"

  external prj :
    ('a, 'b, 'c, 'd, 'e, 'f, 'g, br) app7 -> ('a, 'b, 'c, 'd, 'e, 'f, 'g) t
    = "%identity"
end

module Make8 (X : T8) = struct
  type ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h) t = ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h) X.t
  type br

  external inj :
    ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h) t ->
    ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, br) app8 = "%identity"

  external prj :
    ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, br) app8 ->
    ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h) t = "%identity"
end

(* Standard library brands *)

module Branded = struct
  module Array = Make1 (Stdlib.Array)
  module ArrayLabels = Make1 (Stdlib.ArrayLabels)
  module Hashtbl = Make2 (Stdlib.Hashtbl)
  module Lazy = Make1 (Stdlib.Lazy)
  module List = Make1 (Stdlib.List)
  module ListLabels = Make1 (Stdlib.ListLabels)
  module Option = Make1 (Stdlib.Option)
  module Queue = Make1 (Stdlib.Queue)
  module Result = Make2 (Stdlib.Result)
  module Seq = Make1 (Stdlib.Seq)
  module Stack = Make1 (Stdlib.Stack)
  module Stream = Make1 (Stdlib.Stream)
  module Weak = Make1 (Stdlib.Weak)
end
