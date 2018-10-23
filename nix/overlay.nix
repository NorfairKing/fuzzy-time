final:
  previous:
    with final.haskell.lib;
    {
      fuzzyTimePackages = 
            let fuzzyTimePkg = name:
                (failOnAllWarnings (final.haskellPackages.callCabal2nix name (../. + "/${name}") {}));
            in final.lib.genAttrs [
              "fuzzy-time"
              "fuzzy-time-gen"
            ] fuzzyTimePkg;
      haskellPackages = previous.haskellPackages.override (old: {
        overrides = final.lib.composeExtensions (old.overrides or (_: _: {})) (
          self: super: final.fuzzyTimePackages
        );
      });
    }
