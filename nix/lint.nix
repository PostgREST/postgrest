{ writeShellScriptBin, git, silver-searcher, hlint }:

writeShellScriptBin "postgrest-lint"
  ''
    set -euo pipefail

    rootdir="$(${git}/bin/git rev-parse --show-toplevel)"

    # Lint Haskell files
    ${silver-searcher}/bin/ag -l -g '\.l?hs$' "$rootdir" \
      | xargs ${hlint}/bin/hlint -X QuasiQuotes -X NoPatternSynonyms
  ''
