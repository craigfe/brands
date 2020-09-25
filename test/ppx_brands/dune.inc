; -------- Test: `test.ml` -----------------------------------------------------

; The executable under test
(executable
 (name test)
 (modules test)
 (preprocess (pps ppx_brands))
 (libraries brands))

; Run the PPX on the `.ml` file
(rule
 (targets test.actual)
 (deps
  (:input test.ml))
 (action
  (with-stdout-to
   %{targets}
   (pipe-stdout (run %{exe:../pp.exe} --impl %{input}) (run ocamlformat --impl -)))))

; Compare the post-processed output to the .expected file
(rule
 (alias runtest)
 (action
  (diff test.expected test.actual)))

; Ensure that the post-processed executable runs correctly
(rule
 (alias runtest)
 (action
  (run ./test.exe)))


