let
  nixpkgs = import ./nixpkgs.nix {};
  myHaskellPackages = nixpkgs.pkgs.haskellPackages;
in
{ ... }:
  myHaskellPackages.callCabal2nix "loss-per-line" ./. {}
