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
  hsie =
    runCommand "haskellimports" { inherit name src; }
      ''
        cd $TMP
        cp $src $TMP/Main.hs
        ${ghc}/bin/ghc -O -Werror -Wall -package ghc Main.hs -o Main
        cp Main $out
      '';
  bin =
    runCommand name { inherit hsie name; }
      ''
        mkdir -p $out/bin
        ln -s $hsie $out/bin/$name
      '';
  bashCompletion =
    runCommand "${name}-bash-completion" { inherit bin name; }
      "$bin/bin/$name --bash-completion-script $bin/bin/$name > $out";
in
hsie // { inherit bashCompletion bin; }
