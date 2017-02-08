let
  # inherit (nixpkgs) pkgs;
  channel = "nixos-16.09";
  sysPkgs = import <nixpkgs> {};

  # Use `runCommand` to grab the SHA of the latest build of the channel. This
  # ensures that the `nixpkgs` set we end up with has already passed through
  # Hydra and therefore has passed its tests and has a binary cache available.
  latestRevision = import (sysPkgs.runCommand "latestRevision"
    { buildInputs = [ sysPkgs.wget ];
      # Force the input to be different each time or else Nix won't check for
      # updates to the channel next time we evaluate this expression
      dummy = builtins.currentTime;
    }
    ''
      SSL_CERT_FILE=/etc/ssl/certs/ca-bundle.crt
      # nixos.org/channels/$channel always points to the latest released
      # revision of the channel, which contains a file with its git SHA. Once we
      # have it, we have to wrap it in quotes so it will become a string when we
      # `import` $out
      wget -O - https://nixos.org/channels/${channel}/git-revision |\
        sed 's#\(.*\)#"\1"#' > $out
    '');
  # Now that we have the SHA we can just use Github to get the tarball and thus
  # the `nixpkgs` expressions
    # unstablePkgs = import (fetchTarball "https://github.com/NixOS/nixpkgs-channels/archive/nixpkgs-unstable.tar.gz") {};
# https://github.com/NixOS/nixpkgs.git/commit/e362a3d5c94ba379d428fbd2cc40470719a61556
# pkgs/top-level/release.nix
# haskellPackages.swagger2.x86_64-linux
# src = nixpkgs.fetchgit {
#               url = git://github.com/ghcjs/ghcjs-jquery;
#               rev = "c5eeeafcf81c0d3237b8b9fcb98c4b3633a1297f";
#               sha256 = "3b2de54224963ee17857a9737b65d49edc423e06ad7e9c9b85d9f69ca923676a";
#             };

  config = {
      packageOverrides = pkgs: rec {
        # ghc = pkgs.haskell.packages.ghc7103;
        haskellPackages = pkgs.haskellPackages.override {
          overrides = haskellPackagesNew: haskllPackagesOld: rec {
            swagger2 = haskellPackagesNew.callPackage ./nixExternalLibs/swagger2.nix { };

            hasql-transaction =
              haskellPackagesNew.callPackage ./nixExternalLibs/hasql-transaction.nix { };
          };
        };
      };
      allowUnfree = true;
    };
    pkgs = import (fetchTarball
      "https://github.com/NixOS/nixpkgs-channels/archive/${latestRevision}.tar.gz"
    ) { inherit config; };

    # pkgs = import <nixpkgs> { inherit config; };
in
  { postgrest = pkgs.haskellPackages.callPackage ./postgrest.nix { }; }
