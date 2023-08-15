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

  shellHook = ''
    fish -C "function fish_right_prompt; echo DEV; end"
    exit
  '';
}
