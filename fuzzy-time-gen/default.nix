{ mkDerivation, base, criterion, fuzzy-time, genvalidity
, genvalidity-criterion, genvalidity-hspec, genvalidity-text
, genvalidity-time, hspec, lib, megaparsec, QuickCheck, text, time
}:
mkDerivation {
  pname = "fuzzy-time-gen";
  version = "0.3.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base fuzzy-time genvalidity genvalidity-time QuickCheck
  ];
  testHaskellDepends = [
    base fuzzy-time genvalidity-hspec genvalidity-text hspec megaparsec
    QuickCheck text time
  ];
  benchmarkHaskellDepends = [
    base criterion fuzzy-time genvalidity-criterion
  ];
  homepage = "https://github.com/NorfairKing/fuzzy-time";
  license = lib.licenses.mit;
}
