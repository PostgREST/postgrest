{ lib
, buildPythonPackage
, fetchPypi
, sphinx
}:

buildPythonPackage rec {
  pname = "sphinx-tabs";
  version = "3.2.0";

  src = fetchPypi {
    inherit pname version;
    sha256 = "sha256:1970aahi6sa7c37cpz8nwgdb2xzf21rk6ykdd1m6w9wvxla7j4rk";
  };

  propagatedBuildInputs = [
    sphinx
  ];

  doCheck = false;

  pythonImportsCheck = [ "sphinx_tabs" ];

  meta = with lib; {
    description = "Create tabbed content in Sphinx documentation when building HTML";
    homepage = "https://sphinx-tabs.readthedocs.io";
    license = licenses.mit;
  };
}