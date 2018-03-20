{ mkDerivation, attoparsec, base, bytestring, exceptions
, http-client, mtl, playlists, stdenv, text, transformers
}:
mkDerivation {
  pname = "playlists-http";
  version = "0.2.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base bytestring exceptions http-client mtl playlists
    text transformers
  ];
  homepage = "https://github.com/pjones/playlists-http";
  description = "Library to glue together playlists and http-client";
  license = stdenv.lib.licenses.bsd3;
}
