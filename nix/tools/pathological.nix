{
  fetchFromGitHub,
  postgresql,
  stdenv,
}:

stdenv.mkDerivation {
  pname = "pathological";
  version = "1.0";

  src = fetchFromGitHub {
    owner = "steve-chavez";
    repo = "pg_pathological";
    tag = "v1.0";
    hash = "sha256-X8WowjscyTuWQUqOJN0WNt5ikZ/JiRNCOtFebTOOBT8=";
  };

  nativeBuildInputs = [ postgresql.pg_config ];
  buildInputs = [ postgresql ];

  installPhase = ''
    install -D pathological${postgresql.dlSuffix} -t $out/lib
  '';
}
