{ pkgs ? import <nixpkgs> {}
, nix ? import ../../nix {}
}: rec {
  nativeCommon = [
    pkgs.gprbuild
    pkgs.gnat
    nix.alire
    #nix.libadalang-tools
    #nix.ada-language-server
    #nix.gnatstudio
  ];

  common = [
    pkgs.libarchive
  ];
}
