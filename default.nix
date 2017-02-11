let
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
  config = {
      packageOverrides = pkgs: rec {
        haskellPackages = pkgs.haskellPackages.override {
          overrides = haskellPackagesNew: haskllPackagesOld: rec {
            hasql-transaction =
              haskellPackagesNew.callPackage ./nixExternalLibs/hasql-transaction.nix { };
            swagger2 = haskellPackagesNew.callPackage ./nixExternalLibs/swagger2.nix { };

          };
        };
      };
      allowUnfree = true;
    };
    pkgs = import (fetchTarball
      "https://github.com/NixOS/nixpkgs-channels/archive/${latestRevision}.tar.gz"
    ) { inherit config; };

in
  { postgrest = pkgs.haskellPackages.callPackage ./postgrest.nix { }; }
