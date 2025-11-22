{
  description = "Flake for snake-game in Haskell";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?rev=544961dfcce86422ba200ed9a0b00dd4b1486ec5";
                   # old rev = 7df7ff7d8e00218376575f0acdcc5d66741351ee
  };

  outputs = { self, nixpkgs }: 
  let pkgs = nixpkgs.legacyPackages.x86_64-linux;
  in
  {

    devShells.x86_64-linux.default = import ./shell.nix {inherit pkgs; shell-dir = "brick-tutorial";};

  };
}
