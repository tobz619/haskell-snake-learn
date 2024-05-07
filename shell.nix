{ pkgs ? import <nixpkgs> {}
, shell-dir ? ./. 
}:

let
  pname = builtins.baseNameOf (builtins.toString shell-dir);

in

pkgs.mkShell{

  buildInputs = with pkgs; [
    cabal-install
    haskell.packages.ghc96.haskell-language-server
    cabal2nix
    stack
  ];

  shellHook = '' 
    echo "... updating ${pname}.nix ..."
    cabal2nix ${shell-dir} > ${pname}.nix  
  '';
  
  distribution_nixpkgs_datadir = toString ./distribution-nixpkgs;
}
