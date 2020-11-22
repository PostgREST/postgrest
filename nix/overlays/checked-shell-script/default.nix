self: super:
# Overlay that adds `checkedShellScript`, an enhanced version of
# writeShellScript and writeShellScriptBin
{
  checkedShellScript = super.callPackage ./checked-shell-script.nix { };
}
