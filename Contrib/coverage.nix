{
  writeShellScriptBin,
  buildDotnetGlobalTool,
  dotnet-sdk_8,
}
: let
  report-generator = buildDotnetGlobalTool {
    pname = "reportgenerator";
    nugetName = "dotnet-reportgenerator-globaltool";
    version = "5.1.23";
    nugetSha256 = "sha256-zN/sUYRyeJnHLUsQtH7OTbCeeZ83ZD1SI3nAD/cZut4=";
  };
in
  writeShellScriptBin "coverage" ''
    ${dotnet-sdk_8}/bin/dotnet test --no-build --collect:"XPlat Code Coverage"
    ${report-generator}/bin/reportgenerator reportgenerator -reports:./**/TestResults/*/coverage.cobertura.xml -targetdir:coveragereport -reporttypes:Html
    rm -rf **/TestResults
    $BROWSER coveragereport/index.html
  ''
