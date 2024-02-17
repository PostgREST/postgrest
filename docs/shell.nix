let
  docs =
    import ./default.nix;

  inherit (docs) pkgs;
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

  shellHook = ''
    export HISTFILE=.history
  '';
}
