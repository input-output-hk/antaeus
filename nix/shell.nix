# This file is part of the IOGX template and is documented at the link below:
# https://www.github.com/input-output-hk/iogx#34-nixshellnix

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
    CARDANO_CLI = "${cardano-cli}/bin/cardano-cli";
    CARDANO_NODE = "${cardano-node}/bin/cardano-node";
  };
}
