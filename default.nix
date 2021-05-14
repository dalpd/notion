{ compiler ? "ghc865" }:

let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};

  inherit (pkgs.haskell.lib) dontCheck;

  baseHaskellPkgs = pkgs.haskell.packages.${compiler};

  myHaskellPackages = baseHaskellPkgs.override {
    overrides = hself: hsuper: {
      bastion =
        hself.callCabal2nix
          "bastion"
          (./.)
          {};
    };
  };

  shell = myHaskellPackages.shellFor {
    packages = p: with p; [
      bastion
    ];

    buildInputs = with pkgs.haskellPackages; [
      cabal-install
      ghcid
      ormolu
      hlint
      pkgs.niv
      pkgs.nixpkgs-fmt
    ];

    libraryHaskellDepends = [
    ];

    exactDeps= true;
    withHoogle = false;
    
    shellHook = ''
      set -e
      hpack
      set +e
    '';
};

in
{
  inherit shell;
  bastion = myHaskellPackages.bastion;
}
