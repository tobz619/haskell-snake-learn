{ 
  pkgs
, shell-dir
}:

let
  pname = shell-dir;
  haskell-packages = pkgs.haskellPackages;

in

pkgs.mkShell{

  name = "snake-game";

  imports =  [ ./brick-tutorial.nix ];

  buildInputs = (with haskell-packages; [
    cabal-install
    haskell-language-server
    cabal2nix
    stack
  ]) ++ (with pkgs; [
    bashInteractive
    zlib
    go
    nss
    openssl 
    sqlitebrowser
    caddy
    xcaddy
  ]);

  shellHook = '' 
    echo "... updating ${pname}.nix ..."
    cabal2nix ./. > ${pname}.nix  
    export SHELL=/run/current-system/sw/bin/bash
  '';

  certificateFiles = [ 
    "./.certs/snake_server_auth.crt" 
    "./.certs/snake_server_auth.key"
  ];
  
  distribution_nixpkgs_datadir = toString ./distribution-nixpkgs;
}
