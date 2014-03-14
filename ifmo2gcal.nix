{ cabal, httpConduit, aeson, base16Bytestring, xmlConduit, htmlConduit, text, httpTypes, bytestringShow, time }:

cabal.mkDerivation (self: {
  pname = "ifmo2gcal";
  version = "0.0";
  src = ./src/.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    aeson httpConduit base16Bytestring xmlConduit htmlConduit text httpTypes bytestringShow time
  ];
})
