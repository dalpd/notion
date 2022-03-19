{ # description is a string describing the flake.
  description = "Unofficial Notion Haskell Client";

  # inputs is an attribute set of all the dependencies of the flake.
  inputs = {
    nixpkgs.follows = "haskell-updates";
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
    # Here nixpkgs-unstable:
    nixpkgs-unstable = { url = "github:NixOS/nixpkgs/nixpkgs-unstable"; };
    # And finally haskell-updates:
    haskell-updates = { url = "github:NixOS/nixpkgs/haskell-updates"; };

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
  };

  # outputs is a function of one argument that takes an attribute set of all
  # the realized inputs, and outputs another attribute set.
  outputs = inputs@{ self, nixpkgs, nixpkgs-unstable, flake-compat, ... }:
    # TODO: Since callCabal2nix uses IFD, adding multiple systems to
    # supportedSystems doesn't currently work with flakes. Commands like
    # `nix flake check` and `nix flake show` will fail:
    # > https://github.com/NixOS/nix/issues/4265
    #
    # If it wasn't for that we could use `flake-utils.lib.eachDefaultSystem`
    let
      supportedSystems = [ "x86_64-linux" ];
      config = import ./nix/config.nix;
      forAllSystems = f:
        nixpkgs.lib.genAttrs supportedSystems (system: f system);
      nixpkgsFor = forAllSystems (system:
        import nixpkgs {
          inherit system config;
          overlays = [ self.overlay ];
        });
    in {
      overlay = final: prev:
        let overlays = import ./nix/overlays.nix;
        in prev.lib.composeManyExtensions overlays final prev;
      # Executed by `nix build notion`
      packages = forAllSystems (system:
        let pkgs = nixpkgsFor.${system};
        in { notion = pkgs.notion-with-packages; });

  # Executed by `nix flake check`
  checks = forAllSystems (system: self.packages.${system}.notion);
  # Executed by `nix build .`
  defaultPackage = forAllSystems (system: self.packages.${system}.notion);
  # Used by `nix develop`
  devShell = forAllSystems (system: nixpkgsFor.${system}.notionShell);
  # For when you have an executable:
  #
  # Executed by `nix run . -- <args?>`
  # defaultApp = forAllSystems (system: self.apps.${system}.notion);
  #
  # Executed by `nix run .#<name>`
  # apps = forAllSystems (system: {
  #   notion = {
  #     type = "app";
  #     program = "${self.packages.${system}.notion}/bin/notion"
  #   }
  # });
};
}
