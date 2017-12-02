{ nixpkgs ? import <nixpkgs> { }
, ghc ? nixpkgs.ghc
}:

with nixpkgs;

haskell.lib.buildStackProject {
  name = "playlists-http";

  buildInputs = [
    zlib
  ];

  inherit ghc;
}
