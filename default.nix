{ compiler ? "ghc8107" }:

let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};

  inherit (pkgs.lib) fakeSha256;
  inherit (pkgs.haskell.lib) doJailbreak dontCheck enableCabalFlag packageSourceOverrides;

  baseHaskellPkgs = pkgs.haskell.packages.${compiler};

  myHaskellPackages = baseHaskellPkgs.override {
    overrides = self: super: {
      notion = self.callCabal2nix "notion" (./.) {};

      # Preparation for ghc921
      # blaze-markup = enableCabalFlag super.blaze-markup "--allow-newer=base";
      # cryptohash-sha1 = doJailbreak super.cryptohash-sha1;
      # sop-core = sop-core "0.5.1.0"
    };
  };

  shell = myHaskellPackages.shellFor {
    packages = p: with p; [
      notion
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
{ inherit pkgs;
  inherit shell;
  inherit myHaskellPackages;
  notion = myHaskellPackages.notion;
}
