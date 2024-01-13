{ pkgs ? import <nixpkgs> {}
, nix ? import ../../nix-ada {}
, shared ? import ./shared.nix {}
}:
pkgs.mkShell {
  nativeBuildInputs = shared.nativeCommon;

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
    fish -C "function fish_right_prompt; echo NIX; end"
    exit
  '';
}
