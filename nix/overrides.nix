{ lib, haskell, symlinkJoin, ... }:
with lib;
with haskell.lib;
self: super:
let
  fuzzyTimePackages =
    let fuzzyTimePkg = name: buildStrictly (self.callPackage (../${name}) { });
    in
    genAttrs [
      "fuzzy-time"
      "fuzzy-time-gen"
    ]
      fuzzyTimePkg;
in
{
  inherit fuzzyTimePackages;

  fuzzyTimeRelease = symlinkJoin {
    name = "fuzzyTime-release";
    paths = attrValues self.fuzzyTimePackages;
    passthru = self.fuzzyTimePackages;
  };

} // fuzzyTimePackages
