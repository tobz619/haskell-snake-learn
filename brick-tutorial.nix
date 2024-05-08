{ mkDerivation, base, brick, containers, lib, linear, microlens
, microlens-th, mtl, nonempty-containers, random, vty
, vty-crossplatform
}:
mkDerivation {
  pname = "brick-tutorial";
  version = "0.1.0.0";
  src = /nix/store/0vj8jfkxl2nxzcyidkqvqd5a87iv71bh-brick-tutorial;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base brick containers linear microlens microlens-th mtl
    nonempty-containers random vty vty-crossplatform
  ];
  executableHaskellDepends = [ base brick vty vty-crossplatform ];
  testHaskellDepends = [ base ];
  license = lib.licenses.bsd3;
  mainProgram = "brick-tutorial";
}
