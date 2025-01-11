{ mkDerivation, base, bluefin, brick, containers, lib, linear
, microlens, microlens-mtl, microlens-th, mtl, nonempty-containers
, random, sqlite-simple, text, time, vector, vty, vty-crossplatform
, word-wrap
}:
mkDerivation {
  pname = "brick-tutorial";
  version = "0.1.0.0";
  src = /nix/store/mpf819gxs0dxyanibm8y6aqnf9kmal6q-brick-tutorial;
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
