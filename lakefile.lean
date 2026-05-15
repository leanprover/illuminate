/-
Copyright (c) 2026 Lean FRO LLC. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
Author: David Thrane Christiansen
-/

import Lake
open Lake DSL

package «illuminate» where
  leanOptions := #[⟨`autoImplicit, false⟩, ⟨`linter.missingDocs, true⟩, ⟨`doc.verso, true⟩]

input_dir playerJs where
  text := true
  path := "player_js"

@[default_target]
lean_lib «Illuminate» where
  srcDir := "src"
  needs := #[playerJs]

@[default_target]
lean_lib «IlluminateTests» where
  srcDir := "test"
  leanOptions := #[⟨`linter.missingDocs, false⟩]

@[test_driver]
lean_exe «illuminate-test» where
  srcDir := "test"
  root := `Main
  leanOptions := #[⟨`linter.missingDocs, false⟩]
