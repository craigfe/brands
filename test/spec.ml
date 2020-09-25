open Scaffold

let () =
  v ~this_file:__FILE__ ~libraries:[ "brands" ] ~ppx:"ppx_brands"
    [ ("ppx_brands", ppx_tests ~styler:"ocamlformat --impl -" ()) ]
  |> declare
