{ ghcWithPackages
, runCommand
}:
let
  name = "hsie";
  src = ./Main.hs;
  modules = ps: [
    ps.aeson
    ps.aeson-pretty
    ps.cassava
    ps.dir-traverse
    ps.dot
    ps.ghc-exactprint
    ps.optparse-applicative
  ];
  ghc = ghcWithPackages modules;
  executable =
    runCommand "haskellimports" { inherit name; }
      "${ghc}/bin/ghc -O -Werror -Wall -package ghc ${src} -o $out";
  bin =
    runCommand name { inherit executable name; }
      ''
        mkdir -p $out/bin
        ln -s $executable $out/bin/$name
      '';
  bashCompletion =
    runCommand "${name}-bash-completion" { }
      "${executable} --bash-completion-script ${executable} > $out";
in
executable // { inherit bashCompletion bin; }
