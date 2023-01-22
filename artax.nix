{ mkDerivation, base, sdl2, zlib}:
mkDerivation {
  pname = "artax";
  version = "0.0.0.9";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base sdl2
  ];
}
