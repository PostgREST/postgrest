# Creates an environment that exposes bash-completion arguments from all checkedShellScripts
{ buildEnv }:
{ name
, tools
, extra ? { }
}:
let
  bash-completion = builtins.map (tool: tool.bash-completion) tools;

  env = buildEnv {
    inherit name;
    paths = builtins.map (tool: tool.bin) tools;
  };

in
env // { inherit bash-completion; } // extra
