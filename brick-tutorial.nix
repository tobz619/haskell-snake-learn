{ mkDerivation, base, bluefin, brick, containers, lib, linear
, microlens, microlens-mtl, microlens-th, mtl, nonempty-containers
, random, sqlite-simple, text, time, vector, vty, vty-crossplatform
, word-wrap
}:
mkDerivation {
  pname = "brick-tutorial";
  version = "0.1.0.0";
  src = /nix/store/8fs7hw90afi3a1fm6b5jzlryv1s2m37b-brick-tutorial;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bluefin brick containers linear microlens microlens-mtl
    microlens-th mtl nonempty-containers random sqlite-simple text time
    vector vty vty-crossplatform word-wrap
  ];
  executableHaskellDepends = [ base brick vty vty-crossplatform ];
  testHaskellDepends = [ base ];
  license = lib.licenses.bsd3;
  mainProgram = "brick-tutorial";
}
