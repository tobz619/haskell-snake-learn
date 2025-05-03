{ mkDerivation, async, base, bimap, binary, bluefin, brick
, bytestring, containers, lib, linear, microlens, microlens-mtl
, microlens-th, mtl, network, network-simple-tls
, nonempty-containers, random, scotty, sqlite-simple, stm, text
, time, tls, vector, vty, vty-crossplatform, websockets, word-wrap
, wuss
}:
mkDerivation {
  pname = "brick-tutorial";
  version = "0.1.0.0";
  src = /nix/store/xcccv3cn69n0cdldhfy28b68ljn3ia41-brick-tutorial;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    async base bimap binary bluefin brick bytestring containers linear
    microlens microlens-mtl microlens-th mtl network network-simple-tls
    nonempty-containers random scotty sqlite-simple stm text time tls
    vector vty vty-crossplatform websockets word-wrap wuss
  ];
  executableHaskellDepends = [ base brick vty vty-crossplatform ];
  testHaskellDepends = [ base ];
  license = lib.licenses.bsd3;
}
