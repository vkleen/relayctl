{
  description = "relayd and relayctl";

  inputs = {
    nixpkgs.url = "github:vkleen/nixpkgs";

    haskell-nix.url = "github:input-output-hk/haskell.nix";

    spidev = {
      url = "github:vkleen/spidev";
      flake = false;
    };
  };


  outputs = { self, nixpkgs, haskell-nix, ... }: let
    inherit (nixpkgs) lib;
    pkgs = system: (nixpkgs.legacyPackages."${system}").extend haskell-nix.overlay;

    nonReinstallablePkgs = [
      "Cabal"
       "rts" "ghc-heap" "ghc-prim" "integer-gmp" "integer-simple" "base"
      "deepseq" "array" "ghc-boot-th" "pretty" "template-haskell"
      # ghcjs custom packages
      "ghcjs-prim" "ghcjs-th"
      "ghc-boot"
      "ghc" "Win32" "array" "binary" "bytestring" "containers"
      "directory" "filepath" "ghc-boot" "ghc-compact" "ghc-prim"
      # "ghci" "haskeline"
      "hpc"
      "mtl" "parsec" "process" "text" "time" "transformers"
      "unix" "xhtml"
      "stm" "terminfo"
    ];

    haskell = system: (pkgs system).haskell-nix.stackProject {
      src = ./.;
      sha256map = {
        "https://github.com/vkleen/spidev.git"."a610703e7af18528b4d69389c5e04d0837b5160e" = "1ir9iqap987xkj4j1m938advngwj4qfxbyrv2d7bax07907wqsif";
      };
      pkg-def-extras = [
      ];
      modules = [ ({config, ...}: {
        reinstallableLibGhc = true;
        inherit nonReinstallablePkgs;
        configureFlags = pkgs.lib.optionals pkgs.stdenv.hostPlatform.isMusl [
          "--ghc-option=-optl=-static"
          "--ghc-option=-optl=-pthread"
          "--extra-lib-dirs=${pkgs.gmp.override { withStatic = true; }}/lib"
          "--extra-lib-dirs=${pkgs.zlib_both}/lib"
          "--disable-executable-dynamic"
          "--disable-shared"
          "--enable-executable-stripping"
        ];
      }) ];
    };
  in {
    packages.x86_64-linux.relayctl = (haskell "x86_64-linux").relayctl.components.exes.relayctl;
    packages.x86_64-linux.relayd = (haskell "x86_64-linux").relayctl.components.exes.relayd;

    packages.aarch64-linux.relayd = (haskell "aarch64-linux").relayctl.components.exes.relayd;
  };
}
