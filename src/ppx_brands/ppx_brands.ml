open Ppxlib

module type S = sig
  val inject_type_operators : core_type -> core_type
  val expand_type_decl : rec_flag * type_declaration list -> structure_item list
end

module Located (A : Ast_builder.S) : S = struct
  open A

  let higher x = Ldot (lident "Brands", x)
  let type_apply f a = ptyp_constr (Located.mk (higher "app")) [ a; f ]

  let inject_type_operators typ =
    (object
       inherit [string list] Ast_traverse.fold_map as super

       method! core_type t acc =
         let t, acc = super#core_type t acc in
         match t.ptyp_desc with
         | Ptyp_constr ({ txt = Lident name; _ }, vars) when List.mem name acc
           ->
             ( vars |> List.fold_left (fun t v -> type_apply t v) (ptyp_var name),
               acc )
         | Ptyp_constr (_, vars) ->
             let vars =
               vars
               |> List.filter_map (function
                    | { ptyp_desc = Ptyp_var v; _ } -> Some v
                    | _ -> None)
             in
             (t, vars @ acc)
         | _ -> (t, acc)
    end)
      #core_type
      typ []
    |> fst

  let expand_type_decl (_recflag, tdecls) =
    let tdecl = List.hd tdecls in
    let tdecl = name_type_params_in_td tdecl in
    let params = tdecl.ptype_params in
    let newtype_suffix = List.length params |> string_of_int in
    include_infos
      (pmod_apply
         (pmod_ident (Located.lident ("Brands.Branded.Make" ^ newtype_suffix)))
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
end

let with_engine f ~loc ~path:_ =
  let (module S) = Ast_builder.make loc in
  f (module Located (S) : S)

let str_type_decl =
  Deriving.Generator.make Deriving.Args.empty
    (with_engine (fun (module L) -> L.expand_type_decl))

let extension =
  Extension.declare "b" Extension.Context.Core_type
    Ast_pattern.(__)
    ( with_engine @@ fun (module L) -> function
      | PTyp t -> L.inject_type_operators t | _ -> failwith "invalid" )

let () =
  Reserved_namespaces.reserve "brands";
  Reserved_namespaces.reserve "branded";
  Driver.register_transformation ~extensions:[ extension ] "brands.b";
  Deriving.add ~str_type_decl "brands.branded" |> Deriving.ignore
