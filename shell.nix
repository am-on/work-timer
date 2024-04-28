{
  pkgs ? import <nixpkgs> {}
}:

with pkgs;
stdenv.mkDerivation {
  name = "work-timer";
  buildInputs = [
    elmPackages.create-elm-app
    elmPackages.elm-analyse
    elmPackages.elm-format

    yarn
  ];
  # BUG
  # https://github.com/NixOS/nixpkgs/issues/209668
  NODE_OPTIONS = "--openssl-legacy-provider";
}
