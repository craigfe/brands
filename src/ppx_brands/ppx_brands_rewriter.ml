open Ppxlib

module type S = sig
  val inject_type_operators : core_type -> core_type
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
end

let () =
  let rule =
    Extension.declare "b" Extension.Context.Core_type
      Ast_pattern.(__)
      (fun ~loc ~path:_ ->
        let (module A) = Ast_builder.make loc in
        let (module L) = (module Located (A) : S) in
        function PTyp t -> L.inject_type_operators t | _ -> failwith "invalid")
    |> Context_free.Rule.extension
  in
  Driver.register_transformation ~rules:[ rule ] "b"
