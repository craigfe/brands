# Brands — higher-order polymorphism in OCaml

Convenience types for working with 'defunctionalised' higher-kinded polymorphism
à la '[Lightweight higher-kinded polymorphism][yal14]', Jeremy Yallop and Leo
White (2014).

**STATUS: EXPERIMENTAL**

## Explanation

In OCaml, there is no first-class way to abstract over higher-kinded type
operators without the use of functors:

```ocaml
type all_lists      = 'a list  (* OK! polymorphic over all element types of kind [—] *)
type int_containers = int 'f   (* FAIL! polymorphic over all container types of kind [— ⇒ —] *)
```

This library provides a mechanism to simulate such lightweight higher-kinded
polymorphism:

- define an encoding of type operator application ( `app : — ⇒ — ⇒ —`) in which
  each type operator corresponds to exactly one opaque type called the '_brand_'
  of the operator;

- provide injection and projection functions for type-safe conversion between
  the encoding (e.g. `(int, list_brand) app`) and the true representation (e.g.
  `int list`).

Since the encoding of application uses a simple type in place of a higher-kinded
operator, it can be polymorphic over that operator. For example, we can write
`int 'f` as `(int, 'br) app` where `'br` ranges over the brands of the
corresponding type operators `'f`.

## Related work

This method of achieving higher-kinded polymorphism in OCaml was first proposed
by Jeremy Yallop and Leo White [[Yal14][yal14]]. This implementation is similar
to the one that accompanies the paper ([`ocamllabs/higher`][ocamllabs/higher]),
but differs in several regards:

- Conversions between branded forms and pure forms are implemented using
  extensible variant types rather than a type coercion. (Extensible types were
  not available in OCaml at the time of publication of the paper.)

- Constructed brands are named `br` rather than `t`, to ease inclusion into
  existing modules.

- Canonical brands for types defined in the OCaml standard library are provided.

- The top-level module provides aliases for curried application (`app2`, `app3`
  etc.).

There is also a Jane Street implementation
([`janestreet/higher_kinded`][janestreet/higher_kinded]) of the same ideas, with
pre-provided brands for the types defined in
([`janestreet/base`][janestreet/base]).

## Installation

```
opam pin add --yes https://github.com/CraigFe/brands.git
opam install brands
```

[yal14]:
  https://www.cl.cam.ac.uk/~jdy22/papers/lightweight-higher-kinded-polymorphism.pdf
[ocamllabs/higher]: https://github.com/ocamllabs/higher
[janestreet/higher_kinded]: https://github.com/janestreet/higher_kinded
[janestreet/base]: https://github.com/janestreet/base
