{ inputs, inputs', pkgs, project }:

let 
  cardano-cli = project.hsPkgs.cardano-cli.components.exes.cardano-cli;
  cardano-node = project.hsPkgs.cardano-node.components.exes.cardano-node;
in
{
  name = "antaeus";
  prompt = "\n\\[\\033[1;32m\\][antaeus:\\w]\\$\\[\\033[0m\\] ";
  welcomeMessage = "🤟 \\033[1;31mWelcome to antaeus\\033[0m 🤟";

  packages = [ cardano-cli cardano-node ];

  env = {
    CARDANO_CLI = pkgs.lib.getExe cardano-cli;
    CARDANO_NODE = pkgs.lib.getExe cardano-node; 
  };
}