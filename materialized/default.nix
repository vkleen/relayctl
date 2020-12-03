{
  extras = hackage:
    {
      packages = {
        relayctl = ./relayctl.nix;
        spidev = ./.stack-to-nix.cache.0;
        };
      };
  resolver = "lts-16.12";
  modules = [ ({ lib, ... }: { packages = {}; }) { packages = {}; } ];
  }