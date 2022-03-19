{ nixpkgs ? null
, additionalOverlays ? []
, compiler ? null
, buildExamples ? false
}@args:

(import .nix-helpers/nixpkgs.nix args).notion-with-packages
