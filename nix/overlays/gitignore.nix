self: super:

{
  gitignoreSource =
    let
      gitignoreSrc = super.fetchFromGitHub {
        owner = "hercules-ci";
        repo = "gitignore";
        rev = "2ced4519f865341adcb143c5d668f955a2cb997f";
        sha256 = "sha256:0fc5bgv9syfcblp23y05kkfnpgh3gssz6vn24frs8dzw39algk2z";
      };
    in (super.callPackage gitignoreSrc {}).gitignoreSource;
}
