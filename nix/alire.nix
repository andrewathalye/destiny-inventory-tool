{ pkgs ? import <nixpkgs> {}
, nix ? import ../../nix-ada {}
, shared ? import ./shared.nix {}
}:
pkgs.mkShell {
  nativeBuildInputs = shared.nativeCommon;

  buildInputs = [
    pkgs.pkgconfig
    pkgs.openssl
    pkgs.gmp
    pkgs.gtk3
  ]
  ++ shared.common;

  shellHook = ''
    export NIX_ENFORCE_PURITY=0
    fish -C "function fish_right_prompt; echo ALR; end"
    exit
  '';
}
