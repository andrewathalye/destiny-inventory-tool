{ pkgs ? import <nixpkgs> {}
, nix ? import ../../nix {}
, shared ? import ./shared.nix {}
}:
pkgs.mkShell {
  buildInputs = [
    nix.aws
    nix.vss-stable
    nix.gtkada
    pkgs.gnatcoll-core
    pkgs.gnatcoll-sqlite
  ]
  ++ shared.common;

  shellHook = ''
    export NIX_ENFORCE_PURITY=0
    export GPR_PROJECT_PATH=$GPR_PROJECT_PATH_FOR_TARGET
    export LIBRARY_TYPE=relocatable
    fish -C "function fish_right_prompt; echo DEV; end"
    exit
  '';
}
