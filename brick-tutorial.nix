{ mkDerivation, array, async, base, bimap, binary, bluefin, brick
, bytestring, containers, data-default, data-default-class
, io-streams, io-streams-haproxy, lib, linear, microlens
, microlens-mtl, microlens-th, mtl, network, network-simple-tls
, nonempty-containers, random, scotty, splitmix, sqlite-simple, stm
, text, time, tls, vector, vty, vty-crossplatform, word-wrap
}:
mkDerivation {
  pname = "brick-tutorial";
  version = "0.1.0.0";
  src = /nix/store/bk4sj8bwh36x48zn3y31cc4f729q3h77-brick-tutorial;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    array async base bimap binary bluefin brick bytestring containers
    data-default data-default-class io-streams io-streams-haproxy
    linear microlens microlens-mtl microlens-th mtl network
    network-simple-tls nonempty-containers random scotty splitmix
    sqlite-simple stm text time tls vector vty vty-crossplatform
    word-wrap
  ];
  executableHaskellDepends = [ base brick vty vty-crossplatform ];
  testHaskellDepends = [ base ];
  license = lib.licenses.bsd3;
}
