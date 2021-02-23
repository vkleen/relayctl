{
  description = "relayd and relayctl";

  inputs = {
    haskell-nix.url = "github:input-output-hk/haskell.nix";
    stackageSrc = {
      url = "github:input-output-hk/stackage.nix";
      flake = false;
    };
    hackageSrc = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };
    flake-utils.url = "github:numtide/flake-utils";

    spidev = {
      url = "github:vkleen/spidev";
      flake = false;
    };
  };


  outputs = inputs@{ self, nixpkgs, haskell-nix, ... }: let
    inherit (nixpkgs) lib;
    forAllSystems' = f: lib.mapAttrs f inputs.nixpkgs.legacyPackages;
    forAllSystems = f: lib.mapAttrs f pkgs;

    pkgs = forAllSystems' (system: pkgs: pkgs.appendOverlays (
      [ (haskell-nix.overlays.combined)
        (final: prev: {
          evalPackages = (import final.path {
            overlays = [haskell-nix.overlays.combined-eval-on-build];
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
      ]
    ));

    haskell = forAllSystems (system: p:
      p.haskell-nix.project {
        src = ./.;
        sha256map = {
          "https://github.com/vkleen/spidev.git"."6a2ad9cd12ae04903ae3806d3dc76a3e855ad7ef" = "1z5wyimz8ppxcfx40lzm9j3nl94vj67skhidxz52wkqhv9iflwbm";
        };
        pkg-def-extras = [
        ];
        compiler-nix-name = "ghc8104";
        # materialized = ./materialized;
      });

    devShell = forAllSystems (system: p:
      haskell.${system}.shellFor {
        packages = ps: with ps; [
          relayctl spidev
        ];
        tools = {
          haskell-language-server = "latest";
          cabal = "latest";
          hpack = "latest";
          c2hs = "latest";
          ghcid = "latest";
        };
        buildInputs = [
        ];

        withHoogle = false;

        exactDeps = true;
      });
  in {
    inherit devShell;

    packages.x86_64-linux.relayctl = haskell."x86_64-linux".hsPkgs.relayctl.components.exes.relayctl;
    packages.x86_64-linux.relayd = haskell."x86_64-linux".hsPkgs.relayctl.components.exes.relayd;

    packages.aarch64-linux.relayd = haskell."aarch64-linux".hsPkgs.relayctl.components.exes.relayd;
    packages.aarch64-linux.relayctl = haskell."aarch64-linux".hsPkgs.relayctl.components.exes.relayctl;
  };
}
