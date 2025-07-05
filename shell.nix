{ pkgs ? import <unstable> {}
, shell-dir ? ./. 
}:

let
  pname = builtins.baseNameOf (builtins.toString shell-dir);
  haskell-packages = pkgs.haskellPackages;

in

pkgs.mkShell{

  imports =  [ ./brick-tutorial.nix ];

  buildInputs = (with haskell-packages; [
    cabal-install
    haskell-language-server
    cabal2nix
    stack
  ]) ++ (with pkgs; [
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
    cabal2nix ${shell-dir} > ${pname}.nix  
  '';

  certificateFiles = [ 
    "./.certs/snake_server_auth.crt" 
    "./.certs/snake_server_auth.key"
  ];
  
  distribution_nixpkgs_datadir = toString ./distribution-nixpkgs;
}
