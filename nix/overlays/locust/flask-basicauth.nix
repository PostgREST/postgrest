{ buildPythonPackage
, fetchFromGitHub
, flask
, unittest2
}:

buildPythonPackage rec {
  pname = "flask-basicauth";
  version = "0.2.0";

  src = fetchFromGitHub {
    owner = "jpvanhal";
    repo = "flask-basicauth";
    rev = "v${version}";
    sha256 = "1xkhw5nkhwwzxcz54mr9wzzpx4657scmdim1b2p7km886cxg9ac5";
  };

  propagatedBuildInputs =
    [
      flask
    ];
  checkInputs = [ unittest2 ];

  doCheck = false;

  meta = {
    homepage = "https://github.com/jpvanhal/flask-basicauth";
    description = "HTTP basic access authentication for Flask.";
  };
}
