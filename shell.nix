with import (builtins.fetchGit {
  url = https://github.com/NixOS/nixpkgs-channels;
  ref = "nixos-17.09";
  rev = "14f9ee66e63077539252f8b4550049381a082518";
}) {};
# alternatively you can add 17.09 to your channels with:
#
# nix-channel --add https://nixos.org/channels/nixos-17.09 nixos-17.09
# nix-channel --update
#
# and then remove the above and uncomment this line.
#with (import <nixos-17.09> {});

haskell.lib.buildStackProject {
  name = "postgrest";
  # nixos-17.09 has ghc 8.0.2 by default
  # this is the one used by stack lts-9.6
  buildInputs = [ ghc postgresql zlib pcre ];
}
