{
  description = "A simple nostr library, client and relay";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/677fbe97984e7af3175b6c121f3c39ee5c8d62c9";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = import nixpkgs { inherit system; config.allowUnfree = true; };
      code-coverage-report = pkgs.callPackage ./Contrib/coverage.nix {};
      git-hooks = pkgs.callPackage ./Contrib/trailing-spaces.nix {};
    in {
      packages = {
        default = self.packages.${system}.nostra;

        nostra = pkgs.callPackage ./Contrib/default.nix {
          dotnet-sdk = pkgs.dotnet-sdk_10;
          dotnet-runtime = pkgs.dotnet-runtime_10;
        };
      };

      devShells = with pkgs; {
        default = mkShell {
          name = "nostra-shell";
          packages = [
            dotnet-sdk_10
            nuget-to-json
            sqlite-interactive
            websocat
            git-hooks
            code-coverage-report
            zlib # Aot
            jetbrains.rider
            claude-code
            # Avalonia GUI dependencies
            fontconfig
            freetype
            libGL
            xorg.libX11
            xorg.libXcursor
            xorg.libXi
            xorg.libXrandr
            xorg.libXext
            xorg.libXrender
            xorg.libXinerama
          ];

          DOTNET_ROOT = "${dotnet-sdk_10}";
          DOTNET_CLI_TELEMETRY_OPTOUT = "1";
          DOTNET_NOLOGO = "1";

          shellHook = ''
            export GIT_TOP_LEVEL="$(${pkgs.git}/bin/git rev-parse --show-toplevel)"
            ln -f -s ${git-hooks}/bin/pre-commit $GIT_TOP_LEVEL/.git/hooks/pre-commit
            export PS1='\n\[\033[1;34m\][Nostra:\w]\$\[\033[0m\] '
          '';
        };
      };
    });
}
