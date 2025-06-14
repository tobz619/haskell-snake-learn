{ mkDerivation, array, async, base, bimap, binary, bluefin, brick
, bytestring, containers, lib, linear, microlens, microlens-mtl
, microlens-th, mtl, network, network-simple-tls
, nonempty-containers, random, scotty, splitmix, sqlite-simple, stm
, text, time, tls, vector, vty, vty-crossplatform, word-wrap
}:
mkDerivation {
  pname = "brick-tutorial";
  version = "0.1.0.0";
  src = /nix/store/80vanvgn9ndcpbkl0f9axdqdp9nh47cj-brick-tutorial;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    array async base bimap binary bluefin brick bytestring containers
    linear microlens microlens-mtl microlens-th mtl network
    network-simple-tls nonempty-containers random scotty splitmix
    sqlite-simple stm text time tls vector vty vty-crossplatform
    word-wrap
  ];
  executableHaskellDepends = [ base brick vty vty-crossplatform ];
  testHaskellDepends = [ base ];
  license = lib.licenses.bsd3;
}
