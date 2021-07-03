{ mkDerivation, base, lib, tasty, tasty-hunit }:
mkDerivation {
  pname = "json-parser-haskell";
  version = "0.1.0.0";
  src = ./..;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base tasty tasty-hunit ];
  description = "Simple json parser in Haskell";
  license = lib.licenses.mit;
}
