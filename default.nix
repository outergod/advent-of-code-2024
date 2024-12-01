{ pkgs ? import <nixpkgs> {  } }:

with pkgs;

stdenv.mkDerivation {
  name = "clojure";
  nativeBuildInputs = [
    jdk
    clojure
    clojure-lsp
  ];
  buildInputs = [
  ];
}
