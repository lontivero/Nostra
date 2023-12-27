{
  writeShellScriptBin,
  git,
  gnused,
  gnugrep,
  dotnet-sdk_8,
}:
writeShellScriptBin "pre-commit" ''
    set -e
    IFS="
    "

    for line in `${git}/bin/git diff --check --cached | ${gnused}/bin/sed '/^[+-]/d'` ; do
        FILE="$(echo "$line" | ${gnused}/bin/sed -r 's/:[0-9]+: .*//')"
        mv -f "$FILE" "$FILE.save"
        ${git}/bin/git checkout -- "$FILE"
        ${gnused}/bin/sed -i 's/[[:space:]]*$//' "$FILE"
        ${git}/bin/git add "$FILE"
        ${gnused}/bin/sed 's/[[:space:]]*$//' "$FILE.save" > "$FILE"
        rm "$FILE.save"
    done

    if [ "--`${git}/bin/git status -s | ${gnugrep}/bin/grep '^[A|D|M]'`--" = "----" ]; then
        # empty commit
        echo
        echo -e "\033[31mNO CHANGES ADDED, ABORT COMMIT!\033[0m"
        exit 1
    fi

    TEMP_DIRECTORY=$(mktemp -d)
    SRC_DIRECTORY=$(pwd)

    ${git}/bin/git clone .git $TEMP_DIRECTORY
    ${git}/bin/git diff -P --cached | patch -p1 -d $TEMP_DIRECTORY
    pushd $TEMP_DIRECTORY
    ${dotnet-sdk_8}/bin/dotnet test
    popd
    rm -rf $TEMP_DIRECTORY
''
