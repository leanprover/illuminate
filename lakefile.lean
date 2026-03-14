import Lake
open Lake DSL

package «illuminate» where
  leanOptions := #[⟨`autoImplicit, false⟩, ⟨`linter.missingDocs, true⟩]

@[default_target]
lean_lib «Illuminate» where
  srcDir := "src"

lean_lib «Tests» where
  srcDir := "test"

@[test_driver]
lean_exe «illuminate-test» where
  srcDir := "test"
  root := `Main
