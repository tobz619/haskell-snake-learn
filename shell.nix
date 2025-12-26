{ 
  pkgs
, cabal-pkg
, shell-dir
}:

let
  pname = shell-dir;
  haskell-packages = pkgs.haskellPackages;

in

haskell-packages.shellFor {
  pname = "snake-game";
  
  version = "0.0.1";

  src = ./.;

  packages = hpkgs: [ (hpkgs.callPackage ./brick-tutorial.nix {}) ];

  buildInputs = (with haskell-packages; [
    haskell-language-server
    cabal2nix
    stack
    hie-bios
  ]) ++ (with pkgs; [
    bashInteractive
    zlib
    go
    nss
    openssl 
    sqlitebrowser
    caddy
    xcaddy
  ]) ++
  [ cabal-pkg ] ;

  shellHook = '' 
    echo "... updating ${pname}.nix ..."
    ${pkgs.cabal2nix}/bin/cabal2nix . > ${pname}.nix  
    export SHELL=/run/current-system/sw/bin/bash
    # echo "... generating hie.yaml ..."
    # ${pkgs.haskellPackages.implicit-hie}/bin/gen-hie > hie.yaml
  '';

  certificateFiles = [ 
    "./.certs/snake_server_auth.crt" 
    "./.certs/snake_server_auth.key"
  ];
  
  distribution_nixpkgs_datadir = toString ./distribution-nixpkgs;
}
