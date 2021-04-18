# Creates an environment that exposes bashCompletion arguments from all checkedShellScripts
{ buildEnv }:
{ name
, tools
, extra ? { }
}:
let
  bashCompletion = builtins.map (tool: tool.bashCompletion) tools;

  env = buildEnv {
    inherit name;
    paths = builtins.map (tool: tool.bin) tools;
  };

in
env // { inherit bashCompletion; } // extra
