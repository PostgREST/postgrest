final: prev:
{
  slocat = prev.buildGoModule {
    name = "slocat";
    src = prev.fetchFromGitHub {
      owner = "robx";
      repo = "slocat";
      rev = "52e7512c6029fd00483e41ccce260a3b4b9b3b64";
      sha256 = "sha256-qn6luuh5wqREu3s8RfuMCP5PKdS2WdwPrujRYTpfzQ8=";
    };
    vendorSha256 = "sha256-pQpattmS9VmO3ZIQUFn66az8GSmB4IvYhTTCFn6SUmo=";
  };
}
