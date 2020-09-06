open Ppxlib

let expand_type_decl ~loc ~path:_ (_recflag, tdecls) =
  let (module A) = Ast_builder.make loc in
  let open A in
  let tdecl = List.hd tdecls in
  let tdecl = name_type_params_in_td tdecl in
  let params = tdecl.ptype_params in
  let newtype_suffix =
    match List.length params with 0 -> "" | n -> string_of_int n
  in
  include_infos
    (pmod_apply
       (pmod_ident (Located.lident ("Brands.Make_brand_" ^ newtype_suffix)))
       (pmod_structure
          [
            pstr_type Nonrecursive
              [
                type_declaration ~name:(Located.mk "t") ~params ~cstrs:[]
                  ~kind:Ptype_abstract ~private_:Public
                  ~manifest:
                    (Some
                       (ptyp_constr
                          (Located.map_lident tdecl.ptype_name)
                          (List.map fst params)));
              ];
          ]))
  |> pstr_include
  |> fun str_item -> [ str_item ]

let branded : Deriving.t =
  let open Deriving in
  add ~str_type_decl:(Generator.make Args.empty expand_type_decl) "branded"
