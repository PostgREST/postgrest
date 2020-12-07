let
  docs =
    import ./default.nix;

  pkgs =
    docs.pkgs;
in
pkgs.mkShell {
  name = "postgrest-docs";

  buildInputs = [
    docs.build
    docs.serve
    docs.spellcheck
    docs.dictcheck
    docs.linkcheck
  ];
}
