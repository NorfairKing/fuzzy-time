{ mkDerivation, base, containers, criterion, fuzzy-time
, genvalidity, genvalidity-criterion, genvalidity-hspec
, genvalidity-text, genvalidity-time, hspec, lib, megaparsec
, QuickCheck, text, time
}:
mkDerivation {
  pname = "fuzzy-time-gen";
  version = "0.2.0.1";
  src = ./.;
  libraryHaskellDepends = [
    base containers fuzzy-time genvalidity genvalidity-time megaparsec
    QuickCheck time
  ];
  testHaskellDepends = [
    base fuzzy-time genvalidity genvalidity-hspec genvalidity-text
    genvalidity-time hspec megaparsec QuickCheck text time
  ];
  benchmarkHaskellDepends = [
    base criterion fuzzy-time genvalidity-criterion
  ];
  homepage = "https://github.com/NorfairKing/fuzzy-time";
  license = lib.licenses.mit;
}
