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
      identifier = { name = "relayctl"; version = "0.0.0.0"; };
      license = "BSD-3-Clause";
      copyright = "2020 Viktor Kleen";
      maintainer = "viktor@kleen.org";
      author = "Viktor Kleen";
      homepage = "https://github.com/vkleen/relayctl#readme";
      url = "";
      synopsis = "Control my relays";
      description = "Control my relays";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = "";
      dataFiles = [];
      extraSrcFiles = [];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."generic-lens" or (errorHandler.buildDepError "generic-lens"))
          (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
          (hsPkgs."relude" or (errorHandler.buildDepError "relude"))
          (hsPkgs."servant" or (errorHandler.buildDepError "servant"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."unix" or (errorHandler.buildDepError "unix"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          (hsPkgs."vector-sized" or (errorHandler.buildDepError "vector-sized"))
          ];
        buildable = true;
        modules = [ "Paths_relayctl" "API" ];
        hsSourceDirs = [ "relay-api" ];
        };
      exes = {
        "relayctl" = {
          depends = [
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."ansi-wl-pprint" or (errorHandler.buildDepError "ansi-wl-pprint"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."http-client" or (errorHandler.buildDepError "http-client"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."optparse-generic" or (errorHandler.buildDepError "optparse-generic"))
            (hsPkgs."relayctl" or (errorHandler.buildDepError "relayctl"))
            (hsPkgs."relude" or (errorHandler.buildDepError "relude"))
            (hsPkgs."servant" or (errorHandler.buildDepError "servant"))
            (hsPkgs."servant-client" or (errorHandler.buildDepError "servant-client"))
            (hsPkgs."servant-client-core" or (errorHandler.buildDepError "servant-client-core"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."unix" or (errorHandler.buildDepError "unix"))
            (hsPkgs."vector-sized" or (errorHandler.buildDepError "vector-sized"))
            ];
          buildable = true;
          modules = [ "CLI" "Types" "Paths_relayctl" ];
          hsSourceDirs = [ "relayctl" ];
          mainPath = [ "Main.hs" ];
          };
        "relayd" = {
          depends = [
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."fast-logger" or (errorHandler.buildDepError "fast-logger"))
            (hsPkgs."finite-typelits" or (errorHandler.buildDepError "finite-typelits"))
            (hsPkgs."fixed-vector" or (errorHandler.buildDepError "fixed-vector"))
            (hsPkgs."generic-lens" or (errorHandler.buildDepError "generic-lens"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."relayctl" or (errorHandler.buildDepError "relayctl"))
            (hsPkgs."relude" or (errorHandler.buildDepError "relude"))
            (hsPkgs."safe-exceptions" or (errorHandler.buildDepError "safe-exceptions"))
            (hsPkgs."servant" or (errorHandler.buildDepError "servant"))
            (hsPkgs."servant-server" or (errorHandler.buildDepError "servant-server"))
            (hsPkgs."spidev" or (errorHandler.buildDepError "spidev"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."stm-delay" or (errorHandler.buildDepError "stm-delay"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."unix" or (errorHandler.buildDepError "unix"))
            (hsPkgs."vector-sized" or (errorHandler.buildDepError "vector-sized"))
            (hsPkgs."wai" or (errorHandler.buildDepError "wai"))
            (hsPkgs."warp" or (errorHandler.buildDepError "warp"))
            ];
          buildable = true;
          modules = [
            "Config"
            "Control"
            "LocalConfig"
            "Routes"
            "TLE8108"
            "Paths_relayctl"
            ];
          hsSourceDirs = [ "relayd" ];
          mainPath = [ "Main.hs" ];
          };
        };
      };
    } // rec {
    src = (pkgs.lib).mkDefault ./.;
    }) // { cabal-generator = "hpack"; }