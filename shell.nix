{ pkgs ? import <nixpkgs> {}
, shell-dir ? ./. 
}:

let
  pname = builtins.baseNameOf (builtins.toString shell-dir);
  haskell-packages = pkgs.haskellPackages;

in

pkgs.mkShell{

  imports =  [ ./brick-tutorial.nix ];

  buildInputs = with pkgs; [
    haskell-packages.cabal-install
    haskell-packages.haskell-language-server
    haskell-packages.cabal2nix
    haskell-packages.stack
  ];

  shellHook = '' 
    echo "... updating ${pname}.nix ..."
    cabal2nix ${shell-dir} > ${pname}.nix  
  '';
  
  distribution_nixpkgs_datadir = toString ./distribution-nixpkgs;
}
