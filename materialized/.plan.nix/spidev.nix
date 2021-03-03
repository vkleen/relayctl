{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  ({
    flags = {};
    package = {
      specVersion = "2.2";
      identifier = { name = "spidev"; version = "0.0.0.0"; };
      license = "BSD-3-Clause";
      copyright = "2020 Viktor Kleen";
      maintainer = "viktor@kleen.org";
      author = "Viktor Kleen";
      homepage = "https://github.com/vkleen/relayctl#readme";
      url = "";
      synopsis = "Interface with spidev on Linux";
      description = "";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."finite-typelits" or (errorHandler.buildDepError "finite-typelits"))
          (hsPkgs."inline-c" or (errorHandler.buildDepError "inline-c"))
          (hsPkgs."relude" or (errorHandler.buildDepError "relude"))
          (hsPkgs."unix" or (errorHandler.buildDepError "unix"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."vector-sized" or (errorHandler.buildDepError "vector-sized"))
          ];
        buildable = true;
        modules = [ "Paths_spidev" ];
        hsSourceDirs = [ "src" ];
        };
      };
    } // rec {
    src = (pkgs.lib).mkDefault .././.source-repository-packages/0;
    }) // { cabal-generator = "hpack"; }