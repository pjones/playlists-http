{ pkgs ? import <nixpkgs> { }
}:

let
  nix-hs-src = fetchGit {
    url = "https://code.devalot.com/open/nix-hs.git";
    rev = "2003332a1e8e518b54e6143f9a9467a8a05abca4";
  };

  nix-hs = import "${nix-hs-src}/default.nix" { inherit pkgs; };

in nix-hs {
  cabal = ./playlists-http.cabal;
  flags = [ "build-examples" ];

  overrides = lib: self: super: with lib; {
    http-client =
      if super ? http-client_0_6_2
        then super.http-client_0_6_2
        else super.http-client;
  };
}
