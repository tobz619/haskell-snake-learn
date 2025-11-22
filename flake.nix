{
  description = "Flake for snake-game in Haskell";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs = { self, nixpkgs }: 
  let pkgs = nixpkgs.legacyPackages.x86_64-linux;
  in
  {
    devShells.x86_64-linux.default = import ./shell.nix {inherit pkgs; shell-dir = "brick-tutorial";};

    legacyPackages.x86_64-linux.shell-Packages = pkgs;

  };
}
