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

(** A library for 'defunctionalised' higher-kinded polymorphism in OCaml.

    In OCaml, there is no first-class way to abstract over higher-kinded type
    operators without the use of functors. That is, one can write ['a list] to
    be polymorphic over all types ['a] in a [list] container, but not [int 'f]
    to be polymorphic over all containers ['f] of element type [int].

    This library provides a more lightweight mechanism for higher-kinded
    polymorphism first described by
    \[{{:https://www.cl.cam.ac.uk/~jdy22/papers/lightweight-higher-kinded-polymorphism.pdf}
    Yal14}\]:

    - define an encoding of type operator application ({{!app}
      [app : - => - => -]}) in which each type operator corresponds to exactly
      one opaque type called the '{i brand}' of the operator;

    - provide {{!S1.inj} injection} and {{!S1.prj} projection} functions for
      type-safe conversion between the encoding (e.g. [(int, list_brand) app])
      and the true representation (e.g. [int list]).

    Since the encoding of application uses a simple type in place of a
    higher-kinded operator, it can be polymorphic over that operator. For
    example, we can write [int 'f] as [(int, 'br) app] where ['br] ranges over
    the brands of the corresponding type operators ['f]. *)

include Brands_intf.Brands
(** @inline *)
