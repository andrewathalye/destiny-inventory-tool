{ pkgs ? import <nixpkgs> {}
, nix ? import ../../nix {}
}: rec {
  common = [
    pkgs.gprbuild
    pkgs.gnat
    pkgs.libarchive

    nix.alire
    nix.libadalang-tools
    # nix.gnatstudio
  ];
}
