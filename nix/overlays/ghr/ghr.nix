{ buildGoModule, fetchFromGitHub }:

buildGoModule rec {
  pname = "ghr";
  version = "0.13.0";

  src = fetchFromGitHub {
    rev = "v${version}";
    owner = "tcnksm";
    repo = "ghr";
    sha256 = "1nm5kdjkqayxh06j9nr5daic9sw9nx9w06y9gaqhdrw9byvjpr1a";
  };

  vendorSha256 = "14avsngzhl1b8a05i43ph6sxh9vj0jls0acxr9j7r0h3f0vpamcj";
}
