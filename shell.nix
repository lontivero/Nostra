# shell.nix

with import <nixos> {};
let
  jb = import /home/lontivero/Projects/nixpkgs { config.allowUnfree = true; };
  libs = [
    xorg.libX11
    xorg.libX11.dev
    xorg.libICE
    xorg.libSM
    fontconfig.lib
  ];
  deps = [
    dotnet-sdk_7
    jb.jetbrains.rider
  ];
in
mkShell {
  name = "dotnet-env";
  buildInputs = libs ++ deps;
  DOTNET_ROOT = "${dotnet-sdk_7}";
  LD_LIBRARY_PATH = "${lib.makeLibraryPath libs}";
}
