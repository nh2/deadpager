final:
  previous:
    with final.haskell.lib;
    {
      deadpagePackages = let
        pathFor = name:
          builtins.path {
            inherit name;
            path = ../. + "/${name}";
            filter = path:
              type:
                !final.lib.hasPrefix "." (baseNameOf path);
          };
        deadpagePkg = name:
          dontHaddock (failOnAllWarnings (disableLibraryProfiling
              (addBuildDepends (final.haskellPackages.callCabal2nix name (pathFor name) {}) [final.git final.cacert])));
      in final.lib.genAttrs [
        "deadpage-server"
      ] deadpagePkg;
      haskellPackages = previous.haskellPackages.override (old:
        {
          overrides = final.lib.composeExtensions (old.overrides or (_:
            _:
              {})) (self:
            super:
                {
                classy-prelude = dontHaddock (dontCheck super.classy-prelude);
                classy-prelude-yesod = dontHaddock super.classy-prelude-yesod;
                consul-haskell = addBuildDepend (final.haskellPackages.callCabal2nix "consul-haskell" (final.fetchFromGitHub {
                  owner = "nh2";
                  repo = "consul-haskell";
                  rev = "41499225f1a4fc9d53b7c4753e3fc2c46b81e6bd";
                  sha256 = "sha256:0gp52qzcvlqbqzbg82dr4jlchjcc7l3fc4j3cs5v3f6v3nrxn90y";
                }) {}) (final.consul);
                yesod-static-remote = dontCheck (final.haskellPackages.callCabal2nix "yesod-static-remote" (final.fetchFromGitHub {
                  owner = "NorfairKing";
                  repo = "yesod-static-remote";
                  rev = "22c0a92c1d62f1b8d432003844ef0636a9131b08";
                  sha256 = "sha256:1mz1fb421wccx7mbpn9qaj214w4sl4qali5rclx9fqp685jkfj05";
                }) {});
                } // final.deadpagePackages);
              
        });
    }
