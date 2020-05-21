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
        rev = "2ced4519f865341adcb143c5d668f955a2cb997f";
        sha256 = "sha256:0fc5bgv9syfcblp23y05kkfnpgh3gssz6vn24frs8dzw39algk2z";
      };
    in
    (super.callPackage gitignoreSrc { }).gitignoreSource;
}
