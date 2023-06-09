# This file is part of the IOGX template and is documented at the link below:
# https://www.github.com/input-output-hk/iogx#31-flakenix

{
  description = "End-to-End Testing Framework for Cardano Haskell APIs";


  inputs = {
    iogx.url = "github:input-output-hk/iogx";
  };


  outputs = inputs: inputs.iogx.lib.mkFlake inputs ./.;


  nixConfig = {

    extra-substituters = [
      "https://cache.iog.io"
    ];

    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];

    accept-flake-config = true;

    allow-import-from-derivation = true;
  };
}
