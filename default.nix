{ pkgs ? import (fetchTarball "channel:nixos-23.11") {}
}:

pkgs.haskell.packages.ghc96.developPackage { root = ./.; }
