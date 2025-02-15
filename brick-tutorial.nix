{ mkDerivation, base, bluefin, brick, bytestring, containers, lib
, linear, microlens, microlens-mtl, microlens-th, mtl
, nonempty-containers, random, scotty, sqlite-simple, text, time
, vector, vty, vty-crossplatform, websockets, word-wrap
}:
mkDerivation {
  pname = "brick-tutorial";
  version = "0.1.0.0";
  src = /nix/store/grnxzdqwmanc9y04hbaswfk4gd58kha6-brick-tutorial;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bluefin brick bytestring containers linear microlens
    microlens-mtl microlens-th mtl nonempty-containers random scotty
    sqlite-simple text time vector vty vty-crossplatform websockets
    word-wrap
  ];
  executableHaskellDepends = [ base brick vty vty-crossplatform ];
  testHaskellDepends = [ base ];
  license = lib.licenses.bsd3;
}
