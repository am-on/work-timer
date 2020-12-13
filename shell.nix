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
  ];
}
