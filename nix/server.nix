{ mkDerivation, aeson, base, bytestring, containers, directory, fgl
, file-embed, filepath, foldl, graphviz, hpack, http-types
, open-browser, scotty, stdenv, text, time, turtle, vector
}:
mkDerivation {
  pname = "grbr";
  version = "0.1.0.0";
  src = ../.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring containers directory fgl file-embed filepath
    foldl graphviz http-types open-browser scotty text time turtle
    vector
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base ];
  prePatch = "hpack";
  homepage = "https://github.com/jhrcek/grbr#readme";
  license = stdenv.lib.licenses.bsd3;
}
