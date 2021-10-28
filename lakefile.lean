import Lake
open Lake DSL

package csv {
  libRoots := #[`CSV]
  defaultFacet := PackageFacet.staticLib
}
