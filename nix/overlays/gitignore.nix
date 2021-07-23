self: super:
# Overlay that adds the `gitignoreSource` function from Hercules-CI.
# This function is useful for filtering which files are added to the Nix store.
# See: https://github.com/hercules-ci/gitignore.nix

# To update to a newer revision, the simplest way is to add a new commit hash
# from GitHub under `rev` and to then add the hash that Nix suggests on first
# use.
{
  gitignoreSource =
    let
      gitignoreSrc = super.fetchFromGitHub {
        owner = "hercules-ci";
        repo = "gitignore";
        rev = "211907489e9f198594c0eb0ca9256a1949c9d412";
        sha256 = "06j7wpvj54khw0z10fjyi31kpafkr6hi1k0di13k1xp8kywvfyx8";
      };
    in
    (super.callPackage gitignoreSrc { }).gitignoreSource;
}
