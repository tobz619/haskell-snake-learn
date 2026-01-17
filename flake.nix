{
  description = "Flake for snake-game in Haskell";

  inputs = {
    cabal-pkgset.url = "github:nixos/nixpkgs?ref=nixos-25.05";
    nixpkgs.url = "github:nixos/nixpkgs?rev=89c2b2330e733d6cdb5eae7b899326930c2c0648";
  };

  outputs = { self, nixpkgs, cabal-pkgset }:
    let
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
      cabal-pkg = cabal-pkgset.legacyPackages.x86_64-linux.cabal-install;
    in
    {
      devShells.x86_64-linux.default = import ./shell.nix { inherit pkgs cabal-pkg; shell-dir = "brick-tutorial"; };

      packages.x86_64-linux.default = pkgs.haskellPackages.developPackage {
        root = ./.;
        overrides = self: super: {
          random = pkgs.haskell.lib.overrideCabal super.random {
            version = "1.3.1";
            sha256 = "sha256-2ECsg/JlsM+ipnj47HhifrUM+b4vBnxSyKQjnCm3GjU=";
            doCheck = false;
          };
        };
      };
    };
}
