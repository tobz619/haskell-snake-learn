{
  description = "Flake for snake-game in Haskell";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?rev=544961dfcce86422ba200ed9a0b00dd4b1486ec5";
  };

  outputs = { self, nixpkgs }: 
  let pkgs = nixpkgs.legacyPackages.x86_64-linux;
  in
  {

    devShells.x86_64-linux.default = import ./shell.nix {inherit pkgs; shell-dir = "brick-tutorial";};

  };
}
