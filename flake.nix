{
  description = "relayd and relayctl";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/master";
    haskell-nix = {
      url = "github:input-output-hk/haskell.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    stackageSrc = {
      url = "github:input-output-hk/stackage.nix";
      flake = false;
    };
    hackageSrc = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };
    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    spidev = {
      url = "github:vkleen/spidev";
      flake = false;
    };
  };


  outputs = inputs@{ self, nixpkgs, haskell-nix, ... }: let
    inherit (nixpkgs) lib;
    forAllSystems' = f: lib.mapAttrs f inputs.nixpkgs.legacyPackages;
    forAllSystems = f: lib.mapAttrs f pkgs;

    overlays = system:
      [ (haskell-nix.overlays.combined)
        (final: prev: {
          evalPackages = (import final.path {
            overlays = [haskell-nix.overlays.combined];
            localSystem = system;
          }).buildPackages;
        })
        (final: prev: {
          haskell-nix = prev.haskell-nix // {
            sources = prev.haskell-nix.sources // {
              hackage = inputs.hackageSrc;
              stackage = inputs.stackageSrc;
            };
          };
        })
      ];

    pkgs = forAllSystems' (system: pkgs: pkgs.appendOverlays (overlays system));

    pkgsCross = forAllSystems' (system: pkgs:
      lib.mapAttrs (crossSystem: _: import pkgs.path {
        inherit system;
        crossSystem = lib.systems.examples."${crossSystem}";
        overlays = overlays system;
        crossOverlays = overlays system;
      }) lib.systems.examples
    );

    haskell' = p:
      p.haskell-nix.project {
        src = ./.;
        sha256map = {
          "https://github.com/vkleen/spidev.git"."6a2ad9cd12ae04903ae3806d3dc76a3e855ad7ef" = "1z5wyimz8ppxcfx40lzm9j3nl94vj67skhidxz52wkqhv9iflwbm";
        };
        compiler-nix-name = "ghc8107";
        # materialized = ./materialized;
        # index-state = "2021-02-23T00:00:00Z";
        modules = [
          ({...}: {
            packages.relayctl.components.exes.relayd.enableExecutableProfiling = true;
            packages.relayctl.components.library.enableLibraryProfiling = true;
          })
        ];
      };

    haskell = forAllSystems (_: p: haskell' p);

    devShell = forAllSystems (system: p:
      haskell.${system}.shellFor {
        packages = ps: with ps; [
          relayctl spidev net-mqtt
        ];
        tools = {
          haskell-language-server = "latest";
          cabal = "latest";
          hpack = "latest";
          c2hs = "latest";
          ghcid = "latest";
          fourmolu = "latest";
        };
        buildInputs = [
        ];

        withHoogle = false;

        exactDeps = true;
      });
  in {
    inherit devShell;

    packages = forAllSystems (system: _: {
      relayd = haskell."${system}".hsPkgs.relayctl.components.exes.relayd;

      cross = lib.mapAttrs (crossSystem: _: {
        relayd = (haskell' pkgsCross."${system}"."${crossSystem}").relayctl.components.exes.relayd;
      }) lib.systems.examples;
    });
  };
}
