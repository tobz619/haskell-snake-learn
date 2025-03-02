{ mkDerivation, base, bimap, binary, bluefin, brick, bytestring
, containers, lib, linear, microlens, microlens-mtl, microlens-th
, mtl, nonempty-containers, random, scotty, sqlite-simple, text
, time, vector, vty, vty-crossplatform, websockets, word-wrap
}:
mkDerivation {
  pname = "brick-tutorial";
  version = "0.1.0.0";
  src = /nix/store/a1wh0ghrrvb5vyzh6hyiy3c6qxi7cngc-brick-tutorial;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bimap binary bluefin brick bytestring containers linear
    microlens microlens-mtl microlens-th mtl nonempty-containers random
    scotty sqlite-simple text time vector vty vty-crossplatform
    websockets word-wrap
  ];
  executableHaskellDepends = [ base brick vty vty-crossplatform ];
  testHaskellDepends = [ base ];
  license = lib.licenses.bsd3;
}
