{ pkgs ? import <nixpkgs> {}
, nix ? import ../../nix {}
}:
pkgs.mkShell {
  buildInputs = [
    nix.alire
    # nix.gnatstudio
    pkgs.gprbuild
    pkgs.gnat
    pkgs.gtk3
    pkgs.unzip
    pkgs.pkgconfig
    pkgs.openssl
    pkgs.gmp
    pkgs.libarchive
  ];
}
