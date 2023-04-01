# shell.nix

with import <nixos> {};
let
  ide = import ./ide.nix {};
  packages = [
    dotnet-sdk_7
  ];
  libs = [
    xorg.libX11
    xorg.libX11.dev
    xorg.libICE
    xorg.libSM
    fontconfig.lib
  ];
in
mkShell {
  name = "nostra-dev";
  packages = packages;
  buildInputs = libs ++ ide;

  DOTNET_ROOT = "${dotnet-sdk_7}";
  LD_LIBRARY_PATH = "${lib.makeLibraryPath libs}";
  DOTNET_GLOBAL_TOOLS_PATH = "${builtins.getEnv "HOME"}/.dotnet/tools";

  shellHook = ''
    export PATH="$PATH:$DOTNET_GLOBAL_TOOLS_PATH"
  '';
}
