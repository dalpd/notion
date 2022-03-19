# This file produces a wrapper around Notion that will know where to find a
# GHC with the libraries needed to recompile its config file.
#
# This is not NixOS only; it should work with nix on any system.
#
# There are 4 different ways this file can be used.
#
# 1. build directly with `nix-build`
#
# This method allows you to build notion from the command line with
# `nix-build`.  This is the easiest method.
#
# You can call `nix-build` like the following:
#
# $ nix-build .nix-helpers/notion-with-packages.nix
#
# (This is the same as just calling `nix-build` on the `../default.nix` file.)
#
# This produces a `result` directory that contains the `notion` exectuable as
# `result/bin/notion`.
#
# By default, you will be able to use the Haskell packages `lens` and `colour`
# in your `~/.config/notion/notion.hs` configuration file.
#
# If you want to use alternative Haskell packages, you can specify them on the
# command line:
#
# $ nix-build .nix-helpers/notion-with-packages.nix \
#     --arg extraHaskellPackages 'haskellPackages: [ haskellPackages.colour haskellPackages.lens haskellPackages.pipes ]'
#
# This will make sure you can also use the Haskell packages `lens`, `colour`, and `pipes` in your
# `~/.config/notion/notion.hs` file.  (Actually, if Notion transitively
# depends on a library, you should be able to use it without having to specify
# it.  Although you shouldn't depend on this.)
#
# 2. install with `nix-env`
#
# `nix-env` can be used to install Notion into your environment:
#
# $ nix-env --file .nix-helpers/notion-with-packages.nix --install
#
# If you want to specify extra Haskell packages that you can use in your
# `~/.config/notion/notion.hs` file, you can call `nix-env` like the
# following:
#
# $ nix-env --file .nix-helpers/notion-with-packages.nix --install \
#     --arg extraHaskellPackages 'haskellPackages: [ haskellPackages.colour haskellPackages.lens haskellPackages.pipes ]'
#
# 3. an overlay for your user
#
# Nix makes it easy to create overlays.  First, create the following file at
# `~/.config/nixpkgs/overlays/notion.nix`:
#
# ```nix
# self: super:
# let extraHaskellPackages = hp: [ hp.colour hp.lens hp.MonadRandom ]; in
# { notion = import /some/path/to/notion/.nix-helpers/notion-with-packages.nix { inherit extraHaskellPackages; };
# }
# ```
#
# Now Notion can be installed through Nix's standard methods, including `nix-env`:
#
# $ nix-env --file '<nixpkgs>' --install --attr notion
#
# 4. system-wide in /etc/nixos/configuration.nix
#
# You can set the `nixpkgs.overlays` attribute in your
# `/etc/nixos/configuration.nix` file just like in (3) above.
#
# ```nix
# { config, pkgs, ... }:
# {
#   nixpkgs.overlays = [ (import /home/youruser/.config/nixpkgs/overlays/notion.nix) ];
# }
# ```
#
# You can also add Notion directly to your system packages:
#
# ```nix
# { config, pkgs, ... }:
# {
#   environment.systemPackages = with pkgs; [
#     ...
#     (import /some/path/to/notion/.nix-helpers/notion-with-packages.nix { })
#     ...
#   ];
# }
# ```

{ extraHaskellPackages ? null
, nixpkgs ? null
, additionalOverlays ? []
, compiler ? null
, buildExamples ? false
}@args:

let
  pkgs = import ./nixpkgs.nix {
    inherit compiler nixpkgs additionalOverlays buildExamples extraHaskellPackages;
  };
in

pkgs.notion-with-packages
