
let
  # An overlay that adds notion to all haskell package sets.
  haskellPackagesOverlay = self: super: {
    haskell = super.haskell // {
      packageOverrides = hself: hsuper:
        super.haskell.packageOverrides hself hsuper // {
          notion =
            let
              filesToIgnore = [
                "default.nix"
                "flake.nix"
                "flake.lock"
                ".git"
                ".nix-helpers"
                "result"
                "shell.nix"
              ];

              src =
                builtins.path {
                  # Naming this path makes sure that people will get the same
                  # hash even if they checkout the notion repo into a
                  # directory called something else.
                  name = "notion-src";
                  path = ./..;
                  filter = path: type:
                    with self.lib;
                    ! elem (baseNameOf path) filesToIgnore &&
                    ! any (flip hasPrefix (baseNameOf path)) [ "dist" ".ghc" ];
                };

              extraCabal2nixOptions =
                self.lib.optionalString self.notionBuildExamples "-fbuildexamples";

              notionDrv =
                hself.callCabal2nixWithOptions
                  "notion"
                  src
                  extraCabal2nixOptions
                  {
                  };
            in
            notionDrv;
        };
    };

    # This defines which compiler version is used to build Notion.
    #
    # Either this, or notionKnownWorkingHaskellPkgSet can be changed in an overlay
    # if you want to use a different GHC to build Notion.
    notionCompilerVersion =
      let config = import ./config.nix;
      in config.compiler;

    # A Haskell package set where we know the GHC version works to compile
    # Notion.  This is basically just a shortcut so that other Nix files
    # don't need to figure out the correct compiler version to use when it is
    # not given by the user.
    notionKnownWorkingHaskellPkgSet = self.haskell.packages.${self.notionCompilerVersion};

    # See ./nixpkgs.nix for an explanation of that this does.
    notionBuildExamples = false;

    # See ./nixpkgs.nix for an explanation of that this does.
    notionIndexNotion = false;

    # This is a shell environment for hacking on Notion with cabal.  See the
    # top-level shell.nix for an explanation.
    notionShell =
      let
        # Nix-shell environment for hacking on notion.
        notionEnv = self.notionKnownWorkingHaskellPkgSet.notion.env;

        # Build tools that are nice to have.  It is okay to get Haskell build tools
        # from any Haskell package set, since they do not depend on the GHC version
        # we are using.  We get these from the normal haskellPackages pkg set because
        # then they don't have to be compiled from scratch.
        convenientNativeBuildTools = [
          self.cabal-install
          self.haskellPackages.ghcid
          self.hlint
        ];
      in

      if self.notionIndexNotion
        then
          notionEnv.overrideAttrs (oldAttrs: {
            nativeBuildInputs =
              let
                ghcEnvWithNotion =
                  self.notionKnownWorkingHaskellPkgSet.ghcWithHoogle (hpkgs: [ hpkgs.notion ]);
              in
              oldAttrs.nativeBuildInputs ++ convenientNativeBuildTools ++ [ ghcEnvWithNotion ];
          })
        else
          self.notionKnownWorkingHaskellPkgSet.shellFor {
            withHoogle = true;
            packages = hpkgs: [ hpkgs.notion ];
            nativeBuildInputs = notionEnv.nativeBuildInputs ++ convenientNativeBuildTools;
          };

    # Default Haskell packages that you can use in your Notion configuration.
    # This is only used if the user doesn't specify the extraHaskellPackages
    # option.
    notionExtraHaskellPackages = hpkgs: with hpkgs; [
      colour
      lens
    ];

    notion-with-packages =
      let
        # GHC environment that has notion available, as well as the packages
        # specified above in extraHaskellPackages.
        env =
          self.notionKnownWorkingHaskellPkgSet.ghcWithPackages
            (hpkgs: [ hpkgs.notion ] ++ self.notionExtraHaskellPackages hpkgs);
      in
      self.stdenv.mkDerivation {
        name = "notion-with-packages-ghc-${env.version}";
        buildInputs = [

        ];
        nativeBuildInputs = [];
        dontBuild = true;
        unpackPhase = ":";
        # Using installPhase instead of buildCommand was recommended here:
        # https://github.com/cdepillabout/notion/pull/109
        installPhase = ''
          runHook preInstall
          mkdir -p $out/bin
          ln -sf ${env}/bin/notion $out/bin/notion
          runHook postInstall
        '';
        preferLocalBuild = true;
        allowSubstitutes = false;
      };
  };
in

[ haskellPackagesOverlay ]
