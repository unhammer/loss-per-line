let
  nixpkgs-source = builtins.fetchTarball {
    url = https://github.com/NixOS/nixpkgs/archive/3024ba0b76bf451d38b1ef83be7f4b525671329b.tar.gz;
  };
in
  import nixpkgs-source
