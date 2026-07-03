import Lean.Server.Requests
import Lean.Server.GoTo

open Lean Lsp Server RequestM

initialize gotodefExt :
    SimplePersistentEnvExtension (Name × String) (Array (Name × String)) ←
  registerSimplePersistentEnvExtension {
    addEntryFn    := Array.push
    addImportedFn := fun es => es.foldl (· ++ ·) #[]
  }

def externalPath? (env : Environment) (decl : Name) : Option String :=
  (gotodefExt.getState env).find? (·.1 == decl) |>.map (·.2)

syntax (name := gotodefAttr) "gotodef" str : attr

initialize registerBuiltinAttribute {
  name  := `gotodefAttr
  descr := "add a gotodef entry to the environment"
  add   := fun decl stx _ => do
    let `(attr| gotodef $s:str) := stx | throwError "invalid syntax"
    modifyEnv fun env => gotodefExt.addEntry env (decl, s.getString)
}

def pathToUri (path : String) : IO DocumentUri := do
  let path := if path.startsWith "./" then (path.drop 2).toString else path
  let fp : System.FilePath := path
  let abs ← if fp.isAbsolute then pure fp else pure ((← IO.currentDir) / fp)
  return System.Uri.pathToUri abs.normalize

-- dummy range
def fileStartRange : Range := ⟨⟨0, 0⟩, ⟨0, 0⟩⟩

initialize
  chainLspRequestHandler
      "textDocument/definition"
      TextDocumentPositionParams
      (Array LeanLocationLink)
      fun params oldTask => do
    let doc ← readDoc
    let text := doc.meta.text
    let hoverPos := text.lspPosToUtf8Pos params.position
    bindRequestTaskCostly oldTask fun oldLinks =>
      withWaitFindSnap doc (fun s => s.endPos >= hoverPos)
        (notFoundX := pure oldLinks) fun snap => do
          let env := snap.env
          let mut extra : Array LeanLocationLink := #[]
          let mut seen : Array Name := #[]
          for ll in oldLinks do
            let some ident := ll.ident? | continue
            let decl := ident.decl
            if seen.contains decl then continue
            let some path := externalPath? env decl | continue
            seen := seen.push decl
            let uri ← pathToUri path
            extra := extra.push {
              originSelectionRange? := ll.originSelectionRange?
              targetUri            := uri
              targetRange          := fileStartRange
              targetSelectionRange := fileStartRange
              ident?               := none
              isDefault            := false
            }
          return oldLinks ++ extra

