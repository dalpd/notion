# This file pins the version of nixpkgs to a known good version. The nixpkgs is
# imported with an overlay adding Notion. It is imported from various other
# files.

{ # String representing a GHC version to use.  Normally something like
  # "ghc865".  If null, then use a known-working GHC version.
  compiler ? null
, # A path to nixpkgs.  This will be imported.  If null, use a known-working
  # nixpkgs version.
  nixpkgs ? null
, # Additional overlays to apply when importing nixpkgs.
  additionalOverlays ? []
, # Build all the examples bundled with notion.  Normally this is only used
  # in CI for testing that the examples all still compile.
  buildExamples ? false
, # This is only used for `notionShell`.
  #
  # If this is `true`, Hoogle will also index the Notion libraries,
  # however this will mean the environment will need to be rebuilt every
  # time the notion source changes.
  indexNotion ? false
  # Extra Haskell packages that will be visible by Notion when it compiles
  # itself.  See ./notion-with-packages.nix for an example of how to use
  # this.
, extraHaskellPackages ? null
}:

let
  flake-lock = builtins.fromJSON (builtins.readFile ../flake.lock);

  nixpkgsSrc =
    if isNull nixpkgs
      then
        builtins.fetchTarball {
          url = "https://github.com/NixOS/nixpkgs/archive/${flake-lock.nodes.haskell-updates.locked.rev}.tar.gz";
          sha256 = flake-lock.nodes.haskell-updates.locked.narHash;
        }
      else nixpkgs;

  haskellPackagesOverlays = import ./overlays.nix;

  # This overlays sets some of the options use we at development time.
  notionOptionsOverlay = self: super: {
    notionCompilerVersion =
      if isNull compiler then super.notionCompilerVersion else compiler;

    notionBuildExamples = buildExamples;

    notionIndexNotion = indexNotion;

    notionExtraHaskellPackages =
      if isNull extraHaskellPackages then super.notionExtraHaskellPackages else extraHaskellPackages;
  };

in

import nixpkgsSrc {
  overlays =
    haskellPackagesOverlays ++ [ notionOptionsOverlay ] ++ additionalOverlays;
}
