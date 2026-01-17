{ mkDerivation, array, async, base, bimap, binary, bluefin, brick
, bytestring, containers, crypton-x509, crypton-x509-store
, crypton-x509-system, data-default, data-default-class, free
, http-client, http-media, http-types, io-streams
, io-streams-haproxy, lib, linear, microlens, microlens-mtl
, microlens-th, mtl, network, network-simple-tls
, nonempty-containers, random, scotty, servant-server, splitmix
, sqlite-simple, stm, text, time, tls, tomland, utf8-string, vector
, vty, vty-crossplatform, warp, word-wrap, wreq
}:
mkDerivation {
  pname = "brick-tutorial";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    array async base bimap binary bluefin brick bytestring containers
    crypton-x509 crypton-x509-store crypton-x509-system data-default
    data-default-class free http-client http-media http-types
    io-streams io-streams-haproxy linear microlens microlens-mtl
    microlens-th mtl network network-simple-tls nonempty-containers
    random scotty servant-server splitmix sqlite-simple stm text time
    tls tomland utf8-string vector vty vty-crossplatform warp word-wrap
    wreq
  ];
  executableHaskellDepends = [ base brick vty vty-crossplatform ];
  testHaskellDepends = [ base ];
  license = lib.licenses.bsd3;
}
