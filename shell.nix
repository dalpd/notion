# Running `nix-shell` in the same directory as this file will enter an
# environment in which you can easily hack on or build notion. It provides:
#  * Notion's system dependencies.
#  * GHC with all of notion's haskell dependencies.
#  * Local haddocks for those dependencies.
#  * Hoogle with an index generated from those haddocks.
#  * Cabal.
#  * ghcid.
#  * open-haddock.
#
# In this environment you should be able to e.g.
#  * Build notion:
#    $ cabal new-build
#  * Open a repl with access to the notion libraries:
#    $ cabal new-repl
#  * Use `ghcid`:
#    $ ghcid --command='cabal new-repl'
#  * Open local haddocks for e.g. GI.Vte.Objects.Terminal:
#    $ open-haddock GI.Vte.Objects.Terminal
#    or for the gi-gtk package:
#    $ open-haddock gi-gtk
#
# If you pass nix-shell the arguments `--arg indexNotion true`, then hoogle
# will also index the Notion libraries, however this will mean the environment
# will need to be rebuilt every time the notion source changes.

{ compiler ? null
, indexNotion ? false
, nixpkgs ? null
, buildExamples ? false
, additionalOverlays ? []
}@args:

(import .nix-helpers/nixpkgs.nix args).notionShell
