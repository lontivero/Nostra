# Can be run with:
# nix-build -E 'let pkgs = import <nixpkgs> { }; in pkgs.callPackage ./default.nix {dotnet-sdk = pkgs.dotnet-sdk_7;}' -A passthru.fetch-deps
{
  lib
, buildDotnetModule
, stdenv
, libunwind
, libuuid
, icu
, openssl
, zlib
, curl
, dotnet-sdk
, dotnet-runtime
}:
buildDotnetModule rec {
    inherit dotnet-sdk dotnet-runtime;

    pname = "nostra";
    version = "0.0.1";
    nugetDeps = ./deps.nix; # nix build .#packages.x86_64-linux.default.passthru.fetch-deps

    src = ./..;

    projectFile = ["Nostra.Relay/Nostra.Relay.fsproj" "Nostra.Client/Nostra.Client.fsproj"];
    testProjectFile = "Nostra.Tests/Nostra.Tests.fsproj";
    executables = [ "Nostra.Relay" "Nostra.Client" ];
    doCheck = true;

    # wrap manually, because we want not so ugly executable names
    dontDotnetFixup = true;

    preFixup = ''
      wrapDotnetProgram $out/lib/${pname}/Nostra.Relay $out/bin/nrelay
      wrapDotnetProgram $out/lib/${pname}/Nostra.Client $out/bin/ncli
    '';

    meta = with lib; {
      homepage = "some_homepage";
      description = "The Nostr client and relay.";
      license = licenses.mit;
    };
}
