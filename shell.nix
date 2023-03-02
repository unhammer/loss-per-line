let
  nixpkgs = import ./nixpkgs.nix {};
in
nixpkgs.mkShell {
  name = "loss-per-line";
  inputsFrom = [ (import ./default.nix {}).env ];
}
