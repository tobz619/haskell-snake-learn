{
  description = "Flake for ";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?rev=7df7ff7d8e00218376575f0acdcc5d66741351ee";
  };

  outputs = { self, nixpkgs }: 
  let pkgs = nixpkgs.legacyPackages.x86_64-linux;
  in
  {

    devShells.x86_64-linux.default = import ./shell.nix {inherit pkgs; shell-dir = "brick-tutorial"} ;

  };
}
