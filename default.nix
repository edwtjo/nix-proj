{ mkDerivation, base, stdenv
, ConfigFile, directory, filepath, unix-compat, process }:
mkDerivation {
  pname = "nix-proj";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base ConfigFile directory filepath process unix-compat ];
  description = "Manage Nix project files";
  license = stdenv.lib.licenses.gpl2;
}
