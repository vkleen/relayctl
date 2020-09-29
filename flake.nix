{
  description = "relayd and relayctl";

  inputs = {
    nixpkgs.url = "github:vkleen/nixpkgs";

    haskell-nix.url = "github:input-output-hk/haskell.nix"

    spidev = {
      url = "github:vkleen/spidev";
      flake = false;
    };
  };

  outputs = { self, nixpkgs }: {
    packages.x86_64-linux.hello = nixpkgs.legacyPackages.x86_64-linux.hello;
  };
}
