{ mkDerivation, base, brick, containers, lib, linear, microlens
, microlens-th, mtl, nonempty-containers, random, vty
, vty-crossplatform
}:
mkDerivation {
  pname = "brick-tutorial";
  version = "0.1.0.0";
  src = /nix/store/cch49d45hawpz935j0djw54w7r9pfqk0-brick-tutorial;
  doCheck = false;
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
