{ cabal, httpConduit, aeson, base16Bytestring }:

cabal.mkDerivation (self: {
  pname = "ifmo2gcal";
  version = "0.0";
  src = ./src/.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    aeson httpConduit base16Bytestring 
  ];
})
