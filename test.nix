{ mkDerivation, base, hspec, lib, SHA, split, utf8-string }:
mkDerivation {
  pname = "monad-challenges-code";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  libraryHaskellDepends = [ base split ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base hspec SHA utf8-string ];
  doHaddock = false;
  description = "The best way to learn monads";
  license = lib.licenses.bsd3;
  mainProgram = "main";
}
