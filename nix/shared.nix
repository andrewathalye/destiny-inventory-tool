{ pkgs ? import <nixpkgs> {}
, nix ? import ../../nix-ada {}
}: rec {
  nativeCommon = [
    pkgs.gprbuild
    pkgs.gnat
    pkgs.alire
    #nix.libadalang-tools
    #nix.ada-language-server
    #nix.gnatstudio
  ];

  common = [
    pkgs.libarchive
  ];
}
