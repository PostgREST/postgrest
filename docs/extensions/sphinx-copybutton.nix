{ lib
, buildPythonPackage
, fetchFromGitHub
, sphinx
}:

buildPythonPackage rec {
  pname = "sphinx-copybutton";
  version = "0.4.0";

  src = fetchFromGitHub {
    owner = "executablebooks";
    repo = "sphinx-copybutton";
    rev = "v${version}";
    sha256 = "sha256-vrEIvQeP7AMXSme1PBp0ox5k8Q1rz+1cbHIO+o17Jqc=";
    fetchSubmodules = true;
  };

  propagatedBuildInputs = [
    sphinx
  ];

  doCheck = false; # no tests

  pythonImportsCheck = [ "sphinx_copybutton" ];

  meta = with lib; {
    description = "A small sphinx extension to add a \"copy\" button to code blocks";
    homepage = "https://github.com/executablebooks/sphinx-copybutton";
    license = licenses.mit;
    maintainers = with maintainers; [ Luflosi ];
  };
}
