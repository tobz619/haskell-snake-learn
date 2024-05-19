{ mkDerivation, base, brick, containers, lib, linear, microlens
, microlens-mtl, microlens-th, mtl, nonempty-containers, random
, sqlite-simple, text, time, vector, vty, vty-crossplatform
}:
mkDerivation {
  pname = "brick-tutorial";
  version = "0.1.0.0";
<<<<<<< HEAD
  src = /nix/store/pq5jg2k58yqkinflaqdpkgcrbdlficsq-brick-tutorial;
=======
  src = /nix/store/gg9nhb52br54sclcwznxvm65jgyh83xq-brick-tutorial;
>>>>>>> origin/main
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base brick containers linear microlens microlens-mtl microlens-th
    mtl nonempty-containers random sqlite-simple text time vector vty
    vty-crossplatform
  ];
  executableHaskellDepends = [ base brick vty vty-crossplatform ];
  testHaskellDepends = [ base ];
  license = lib.licenses.bsd3;
  mainProgram = "brick-tutorial";
}
