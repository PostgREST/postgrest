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
        rev = "c4662e662462e7bf3c2a968483478a665d00e717";
        sha256 = "1npnx0h6bd0d7ql93ka7azhj40zgjp815fw2r6smg8ch9p7mzdlx";
      };
    in
    (super.callPackage gitignoreSrc { }).gitignoreSource;
}
