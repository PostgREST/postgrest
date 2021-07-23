{ buildGoModule, fetchFromGitHub }:

buildGoModule rec {
  pname = "ghr";
  version = "0.14.0";

  src = fetchFromGitHub {
    rev = "v${version}";
    owner = "tcnksm";
    repo = "ghr";
    sha256 = "1jjc3bwmyw831r1ayic1f1ysh5ggm88aszbndm0swg8byhz56pd4";
  };

  vendorSha256 = "06cbhsnxv4gisnwrhw61af7rpv2a9slf9z2wbn79r91xzkh51vzr";

  # Disabling tests, as they require a GitHub API token
  doCheck = false;
}
