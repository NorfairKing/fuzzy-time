final:
previous:
with final.haskell.lib;
{
  fuzzyTimePackages =
    let fuzzyTimePkg = name:
      (buildStrictly (final.haskellPackages.callCabal2nixWithOptions name (final.gitignoreSource (../. + "/${name}")) "--no-hpack" { }));
    in
    final.lib.genAttrs [
      "fuzzy-time"
      "fuzzy-time-gen"
    ]
      fuzzyTimePkg;
  haskellPackages = previous.haskellPackages.override (old: {
    overrides = final.lib.composeExtensions (old.overrides or (_: _: { })) (
      self: super: final.fuzzyTimePackages
    );
  });
}
