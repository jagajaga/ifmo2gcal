{ cabal, httpConduit, aeson, base16Bytestring, xmlConduit, htmlConduit, text }:

cabal.mkDerivation (self: {
  pname = "ifmo2gcal";
  version = "0.0";
  src = ./src/.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    aeson httpConduit base16Bytestring xmlConduit htmlConduit text 
  ];
})
