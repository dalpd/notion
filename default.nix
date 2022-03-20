{ nixpkgs ? null
, additionalOverlays ? []
, compiler ? null
}@args:

(import .nix-helpers/nixpkgs.nix args).notion-with-packages
