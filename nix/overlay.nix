final: prev:
with final.lib;
with final.haskell.lib;
{
  fuzzyTimeRelease = final.symlinkJoin {
    name = "fuzzy-time-release";
    paths = attrValues final.haskellPackages.fuzzyTimePackages;
  };

  haskellPackages = prev.haskellPackages.override (old: {
    overrides = composeExtensions (old.overrides or (_: _: { })) (
      self: super:
        let
          fuzzyTimePackages =
            let fuzzyTimePkg = name: buildStrictly (self.callPackage (../${name}) { });
            in
            final.lib.genAttrs [
              "fuzzy-time"
              "fuzzy-time-gen"
            ]
              fuzzyTimePkg;
        in
        { inherit fuzzyTimePackages; } // fuzzyTimePackages
    );
  });
}
