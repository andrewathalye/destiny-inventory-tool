{ pkgs ? import <nixpkgs> {}
, nix ? import ../../nix {}
, shared ? import ./shared.nix {}
}:
pkgs.mkShell {
  buildInputs = [
    pkgs.pkgconfig
    pkgs.openssl
    pkgs.gmp
    pkgs.gtk3
  ]
  ++ shared.common;
}
