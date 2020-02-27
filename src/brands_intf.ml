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

module type T1 = sig
  type _ t
end

module type T2 = sig
  type (_, _) t
end

module type T3 = sig
  type (_, _, _) t
end

module type T4 = sig
  type (_, _, _, _) t
end

module type T5 = sig
  type (_, _, _, _, _) t
end

module type T6 = sig
  type (_, _, _, _, _, _) t
end

module type T7 = sig
  type (_, _, _, _, _, _, _) t
end

module type T8 = sig
  type (_, _, _, _, _, _, _, _) t
end

(* module Make0 (X: sig type t end) () : S0 with type t = X.t *)

module type Brands = sig
  (**  *)

  (** {1 Type operator application} *)

  type ('a, 'fn) app = ..
  (** Application of type operators. The type [('a, br) app] encodes the type
      ['a f] where [br] is the brand of the type operator [f].

      Type operator application is right-associative, so the type
      [('c, 'b, 'a) f] is encoded as [('c, ('b, ('a, br) app) app) app]. Or,
      using the {{!curried} curried aliases} defined below, as
      [('c, 'b, 'a, br) app3]. *)

  (** {1 Related work}

      This method of achieving higher-kinded polymorphism in OCaml was first
      proposed by Jeremy Yallop and Leo White in
      {{:https://www.cl.cam.ac.uk/~jdy22/papers/lightweight-higher-kinded-polymorphism.pdf}
      'Lightweight higher-kinded polymorphism}', 2014. This implementation is
      similar to the one that accompanies the paper
      ({{:https://github.com/ocamllabs/higher} [ocamllabs/higher]}), but differs
      in several regards:

      - Conversions between branded forms and pure forms are implemented using
        extensible variant types rather than a type coercion. (Extensible types
        were not available in OCaml at the time of publication of the paper.)

      - Constructed brands are named [br] rather than [t], to ease inclusion
        into existing modules.

      - Canonical brands for types defined in the OCaml standard library are
        provided.

      - The top-level module provides aliases for curried application ({!app2},
        {!app3} etc.).

      There is also a Jane Street implementation
      ({{:https://github.com/janestreet/higher_kinded}
      [janestreet/higher_kinded]}) of the same ideas, with pre-provided brands
      for the types defined in {{:https://github.com/janestreet/base}
      [janestreet/base]}. *)

  (** {1:curried Curried syntax} *)

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

  (** {1 Branded types} *)

  (** The interfaces of branded types of arities from 1 to 8. *)

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

  (** {2 [Stdlib] brands}

      Brands for types defined in the OCaml standard library. *)

  module Branded : sig
    module Array : S1 with type 'a t := 'a Stdlib.Array.t

    module ArrayLabels : S1 with type 'a t := 'a Stdlib.ArrayLabels.t

    module Hashtbl : S2 with type ('a, 'b) t := ('a, 'b) Stdlib.Hashtbl.t

    module Lazy : S1 with type 'a t := 'a Stdlib.Lazy.t

    module List : S1 with type 'a t := 'a Stdlib.List.t

    module ListLabels : S1 with type 'a t := 'a Stdlib.ListLabels.t

    module Option : S1 with type 'a t := 'a Stdlib.Option.t

    module Queue : S1 with type 'a t := 'a Stdlib.Queue.t

    module Result : S2 with type ('a, 'b) t := ('a, 'b) Stdlib.Result.t

    module Seq : S1 with type 'a t := 'a Stdlib.Seq.t

    module Stack : S1 with type 'a t := 'a Stdlib.Stack.t

    module Stream : S1 with type 'a t := 'a Stdlib.Stream.t

    module Weak : S1 with type 'a t := 'a Stdlib.Weak.t
  end

  (** {2 Constructing brands} *)

  module Make_brand1 (X : T1) () : S1 with type 'a t := 'a X.t

  module Make_brand2 (X : T2) () : S2 with type ('a, 'b) t := ('a, 'b) X.t

  module Make_brand3 (X : T3) () :
    S3 with type ('a, 'b, 'c) t := ('a, 'b, 'c) X.t

  module Make_brand4 (X : T4) () :
    S4 with type ('a, 'b, 'c, 'd) t := ('a, 'b, 'c, 'd) X.t

  module Make_brand5 (X : T5) () :
    S5 with type ('a, 'b, 'c, 'd, 'e) t := ('a, 'b, 'c, 'd, 'e) X.t

  module Make_brand6 (X : T6) () :
    S6 with type ('a, 'b, 'c, 'd, 'e, 'f) t := ('a, 'b, 'c, 'd, 'e, 'f) X.t

  module Make_brand7 (X : T7) () :
    S7
      with type ('a, 'b, 'c, 'd, 'e, 'f, 'g) t :=
            ('a, 'b, 'c, 'd, 'e, 'f, 'g) X.t

  module Make_brand8 (X : T8) () :
    S8
      with type ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h) t :=
            ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h) X.t
end
