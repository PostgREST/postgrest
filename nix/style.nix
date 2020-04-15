{ writeShellScriptBin
, git
, nixpkgs-fmt
, silver-searcher
, stylish-haskell
}:

writeShellScriptBin "postgrest-style"
  ''
    set -euo pipefail

    rootdir="$(${git}/bin/git rev-parse --show-toplevel)"

    # Format Nix files
    ${nixpkgs-fmt}/bin/nixpkgs-fmt "$rootdir" > /dev/null 2> /dev/null

    # Format Haskell files
    ${silver-searcher}/bin/ag -l -g '\.l?hs$' "$rootdir" \
      | xargs ${stylish-haskell}/bin/stylish-haskell -i
  ''
