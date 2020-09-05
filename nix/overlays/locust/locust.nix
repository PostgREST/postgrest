{ buildPythonPackage
, ConfigArgParse
, fetchFromGitHub
, flask
, flask-basicauth
, gevent
, geventhttpclient
, mock
, msgpack
, psutil
, pyquery
, pyzmq
, requests
, unittest2
}:

buildPythonPackage rec {
  pname = "locustio";
  version = "1.2.3";

  src = fetchFromGitHub {
    owner = "locustio";
    repo = "locust";
    rev = version;
    sha256 = "11y28hi28m98dl26v9wxx4nl5756vpfgvg4crpsm93hrx7w8043q";
  };

  propagatedBuildInputs =
    [
      ConfigArgParse
      flask
      flask-basicauth
      gevent
      geventhttpclient
      msgpack
      psutil
      pyquery
      pyzmq
      requests
    ];
  checkInputs = [ mock unittest2 ];
  # remove file which attempts to do GET request
  preCheck = ''
    rm locust/test/test_stats.py
  '';

  # Running tests will take some more work
  doCheck = false;

  meta = {
    homepage = "https://locust.io/";
    description = "Scalable user load testing tool written in Python";
  };
}
