{ mkDerivation, base, containers, deepseq, lib, megaparsec, text
, time, validity, validity-time
}:
mkDerivation {
  pname = "fuzzy-time";
  version = "0.3.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers deepseq megaparsec text time validity validity-time
  ];
  homepage = "https://github.com/NorfairKing/fuzzy-time";
  license = lib.licenses.mit;
}
