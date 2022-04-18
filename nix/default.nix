{ mkDerivation, aeson, base, bytestring, directory, filepath
, hasktorch, http-conduit, lib, mtl, network-uri
, optparse-applicative, optparse-generic, pretty-simple, process
, scalpel, servant-server, servant-swagger, sqlite-simple, swagger2
, tagsoup, text, time, wai-cors, wai-logger, warp
}:
mkDerivation {
  pname = "openmemex";
  version = "0.1.0.0";
  src = ../.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring directory filepath hasktorch http-conduit mtl
    network-uri optparse-applicative optparse-generic pretty-simple
    process scalpel servant-server servant-swagger sqlite-simple
    swagger2 tagsoup text time wai-cors wai-logger warp
  ];
  homepage = "https://github.com/austinvhuang/openmemex#readme";
  license = lib.licenses.asl20;
}
