{ sources ? import ./nix/sources.nix, pkgs ? import sources.nixpkgs { }
, nix-hs ? import sources.nix-hs { inherit pkgs; }, compiler ? "default" }:

nix-hs {
  inherit compiler;

  cabal = ./playlists-http.cabal;
  flags = [ "build-examples" ];

  overrides = lib: self: super: {
    playlists = import sources.playlists { inherit pkgs nix-hs compiler; };
  };
}
