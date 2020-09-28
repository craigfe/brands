open Ppxlib

module type S = sig
  val inject_type_operators : core_type -> core_type

  val expand_str_type_decl :
    rec_flag * type_declaration list -> structure_item list

  val expand_sig_type_decl :
    rec_flag * type_declaration list -> signature_item list
end

let branded_maker = Printf.sprintf "Brands.Branded.Make%d"
let branded_mtyp = Printf.sprintf "Brands.Branded.S%d"

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

  let expand_str_type_decl (_recflag, tdecls) =
    let tdecl = List.hd tdecls in
    let tdecl = name_type_params_in_td tdecl in
    let params = tdecl.ptype_params in
    include_infos
      (pmod_apply
         (pmod_ident (Located.lident (branded_maker (List.length params))))
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

  let expand_sig_type_decl (_recflag, tdecls) =
    let tdecl = List.hd tdecls in
    let tdecl = name_type_params_in_td tdecl in
    let for_subst =
      type_declaration ~name:tdecl.ptype_name ~params:tdecl.ptype_params
        ~cstrs:[] ~private_:Public ~kind:Ptype_abstract
        ~manifest:
          (Some
             (ptyp_constr
                (Located.map_lident tdecl.ptype_name)
                (List.map fst tdecl.ptype_params)))
    in
    let with_constraints =
      [ Pwith_typesubst (Located.lident "t", for_subst) ]
    in
    include_infos
      (pmty_with
         (pmty_ident
            (Located.lident (branded_mtyp (List.length tdecl.ptype_params))))
         with_constraints)
    |> psig_include
    |> fun sig_item -> [ sig_item ]
end

let with_engine f ~loc ~path:_ =
  let (module S) = Ast_builder.make loc in
  f (module Located (S) : S)

let sig_type_decl =
  Deriving.Generator.make Deriving.Args.empty
    (with_engine (fun (module L) -> L.expand_sig_type_decl))

let str_type_decl =
  Deriving.Generator.make Deriving.Args.empty
    (with_engine (fun (module L) -> L.expand_str_type_decl))

let extension =
  Extension.declare "b" Extension.Context.Core_type
    Ast_pattern.(__)
    ( with_engine @@ fun (module L) -> function
      | PTyp t -> L.inject_type_operators t | _ -> failwith "invalid" )

let () =
  Reserved_namespaces.reserve "brands";
  Reserved_namespaces.reserve "branded";
  Driver.register_transformation ~extensions:[ extension ] "brands.b";
  Deriving.add ~str_type_decl ~sig_type_decl "branded" |> Deriving.ignore
