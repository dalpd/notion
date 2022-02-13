{ # description is a string describing the flake.
  description = "Unofficial Notion Haskell Client";

  # inputs is an attribute set of all the dependencies of the flake.
  inputs = {
    nixpkgs.follows = "nixpkgs-release";
    # The following:
    # > nixpkgs.url = "github:NixOS/nixpkgs"
    # points nixpkgs to the latest commit on the default branch but we can
    # also use a specific hash to pin the package set to a specific commit,
    # like so:
    # > nixpkgs.url = "github:nixos/nixpkgs/<hash>"
    # or rather a specific branch, e.g.:
    # > nixpkgs.url = "github:nixos/nixpkgs/<branch>"
    # Here we declare the release branch we want to track:
    nixpkgs-release = { url = "github:NixOS/nixpkgs/release-21.11"; };
    # Defining in case we end up needing packages from nixpkgs-unstable.
    nixpkgs-unstable = { url = "github:NixOS/nixpkgs/nixpkgs-unstable"; };

    # Pure Nix flake utility functions:
    # > https://github.com/numtide/flake-utils
    flake-utils = { url = "github:numtide/flake-utils"; };

    # Compatibility function to allow flakes to be used by non-flake-enabled
    # Nix versions:
    # > https://github.com/edolstra/flake-compat
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };

    servant = {
      url = "github:haskell-servant/servant";
      flake = false;
    };
  };

  # outputs is a function of one argument that takes an attribute set of all
  # the realized inputs, and outputs another attribute set.
  outputs =
    inputs@{ self, nixpkgs, nixpkgs-unstable, flake-compat, flake-utils, ... }:
    # TODO: Since callCabal2nix uses IFD, adding multiple systems to
    # supportedSystems doesn't currently work with flakes. Commands like
    # `nix flake check` and `nix flake show` will fail:
    # > https://github.com/NixOS/nix/issues/4265
    #
    # So alternatively we could just provide the following:
    # > supportedSystems = [ "x86_64-linux" ];
    flake-utils.lib.eachDefaultSystem (system:
      let config = import ./nix/config.nix;
          version = builtins.substring 0 8 self.lastModifiedDate;
          overlays = final: prev:
            let overlays = import ./nix/overlays;
            in prev.lib.composeManyExtensions overlays final prev;
          pkgs = import nixpkgs { inherit system overlays config; };
          haskellPackages = pkgs.haskell.packages.${config.compiler};
    in
      {
      };

}
