{ mkDerivation, base, bytestring, hspec, parsers, raw-strings-qq
, stdenv, text, trifecta
}:
mkDerivation {
  pname = "addresses";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring hspec parsers raw-strings-qq text trifecta
  ];
  testHaskellDepends = [
    base bytestring hspec parsers raw-strings-qq text trifecta
  ];
  homepage = "https://github.com/dmvianna/addresses.git#readme";
  description = "parsers for commonly used Australian street address types";
  license = stdenv.lib.licenses.bsd3;
}
