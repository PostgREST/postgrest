{ compiler
, name
, pkgs
, src
}:
let
  # This builds a static PostgREST executable based on pkgsStatic.
  inherit (pkgs) pkgsStatic;
  inherit (pkgsStatic.haskell) lib;

  packagesStatic =
    let
      base = pkgsStatic.haskell.packages.native-bignum."${compiler}";
      thCyclePackages = [
        "network"
        "random"
        "splitmix"
        "temporary"
        "character-ps"
        "hashable"
        "data-fix"
        "dlist"
        "generically"
        "indexed-traversable"
        "primitive"
        "integer-conversion"
        "integer-logarithms"
        "th-compat"
        "network-uri"
        "OneTuple"
        "QuickCheck"
        "scientific"
        "tagged"
        "unordered-containers"
        "colour"
        "ansi-terminal-types"
        "ansi-terminal"
        "prettyprinter"
        "prettyprinter-ansi-terminal"
        "transformers-compat"
        "optparse-applicative"
        "tasty"
        "vector-stream"
        "base-orphans"
        "code-page"
        "ghc-paths"
        "syb"
        "doctest"
        "call-stack"
        "tasty-hunit"
        "inspection-testing"
        "tasty-inspection-testing"
        "tasty-quickcheck"
        "vector"
        "indexed-traversable-instances"
        "assoc"
        "distributive"
        "comonad"
        "th-abstraction"
        "bifunctors"
        "StateVar"
        "contravariant"
        "semigroupoids"
        "these"
        "semialign"
        "strict"
        "time-compat"
        "text-iso8601"
        "text-short"
        "uuid-types"
        "witherable"
        "aeson"
        "iserv-proxy"
      ];
    in
    base.extend (_: prev: {
      # Work around nixpkgs cross/static eval cycles around iserv-proxy.
      mkDerivation = args:
        prev.mkDerivation (args // {
          enableExternalInterpreter =
            if pkgs.lib.elem args.pname thCyclePackages
            then false
            else args.enableExternalInterpreter or true;
        });
    });

  makeExecutableStatic = drv: pkgs.lib.pipe drv [
    lib.compose.justStaticExecutables

    # To successfully compile a redistributable, fully static executable we need to:
    # 1. avoid any references to /nix/store to prevent blowing up the closure size.
    (drv: drv.overrideAttrs {
      allowedReferences = [
        pkgsStatic.openssl.etc
      ];
    })

    # 2. be able to run the executable.
    (drv: drv.overrideAttrs {
      passthru.tests.version = pkgsStatic.testers.testVersion {
        package = drv;
      };
    })
  ];

in
{
  inherit packagesStatic;

  postgrestStatic = makeExecutableStatic (packagesStatic.callCabal2nix name src { });
}
