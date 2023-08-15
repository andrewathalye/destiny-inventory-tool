{ pkgs ? import <nixpkgs> {}
, nix ? import ../../nix {}
}:
pkgs.mkShell {
  buildInputs = [
    # nix.gnatstudio
    nix.alire
    nix.aws
    nix.vss-stable
    nix.gtkada
    pkgs.libarchive
    pkgs.gnat
    pkgs.gprbuild
    pkgs.gnatcoll-core
    pkgs.gnatcoll-sqlite
  ];
}
