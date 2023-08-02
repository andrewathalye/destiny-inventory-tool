{pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  buildInputs = [
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
