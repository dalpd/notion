{ extraHaskellPackages ? null
, nixpkgs ? null
, additionalOverlays ? []
, compiler ? null
}@args:

let
  pkgs = import ./nixpkgs.nix {
    inherit compiler nixpkgs additionalOverlays extraHaskellPackages;
  };
in

pkgs.notion-with-packages
