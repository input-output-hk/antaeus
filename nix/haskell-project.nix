# The haskell.nix project

{
  # Desystemized merged inputs.
  # All the inputs from iogx (e.g. CHaP, haskell-nix, etc..) unioned with the
  # inputs defined in your flake. You will also find the `self` attribute here.
  # These inputs have been desystemized against the current system.
  inputs

  # Non-desystemized merged inputs.
  # All the inputs from iogx (e.g. CHaP, haskell-nix, etc..) unioned with the
  # inputs defined in your flake. You will also find the `self` argument here.
  # These inputs have not been desystemized, they are the original `inputs` from
  # iogx and your `flake.nix`.
, systemized-inputs

  # The very attrset passed to `inputs.iogx.mkFlake` in your `flake.nix`.
, flakeopts

  # Desystemized legacy nix packages configured against `haskell.nix`.
  # NEVER use the `nixpkgs` coming from `inputs` or `systemized-inputs`!
, pkgs

  # The current compiler against which to build the haskell.nix project.
  # You want to set this as the value of `compiler-nix-name`.
, ghc

  # This flag will be set to false when building for CI, and will be set to true
  # when the build has to include haddock.
, deferPluginErrors

  # Whether to enable haskell profiling.
, enableProfiling
}:

let
  lib = pkgs.lib;
  isDarwin = pkgs.stdenv.hostPlatform.isDarwin;
  isCross = pkgs.stdenv.hostPlatform != pkgs.stdenv.buildPlatform;

  antaeus-module = { config, ... }: {
    packages = {
      # Things that need plutus-tx-plugin
      freer-extras.package.buildable = !isCross;
      e2e-tests.package.buildable = !isCross;
      # These need R
      plutus-core.components.benchmarks.cost-model-test.buildable = lib.mkForce (!isCross);
      plutus-core.components.benchmarks.update-cost-model.buildable = lib.mkForce (!isCross);

      e2e-tests.doHaddock = deferPluginErrors;
      e2e-tests.flags.defer-plugin-errors = deferPluginErrors;

      # The lines `export CARDANO_NODE=...` and `export CARDANO_CLI=...`
      # is necessary to prevent the error
      # `../dist-newstyle/cache/plan.json: openBinaryFile: does not exist (No such file or directory)`.
      # See https://github.com/input-output-hk/cardano-node/issues/4194.
      #
      # The line 'export CARDANO_NODE_SRC=...' is used to specify the
      # root folder used to fetch the `configuration.yaml` file (in
      # antaeus, it's currently in the
      # `configuration/defaults/byron-mainnet` directory.
      # Else, we'll get the error
      # `/nix/store/ls0ky8x6zi3fkxrv7n4vs4x9czcqh1pb-antaeus/antaeus/test/configuration.yaml: openFile: does not exist (No such file or directory)`
      e2e-tests.components.tests.antaeus-test.preCheck = "
        export CARDANO_CLI=${config.hsPkgs.cardano-cli.components.exes.cardano-cli}/bin/cardano-cli${pkgs.stdenv.hostPlatform.extensions.executable}
        export CARDANO_NODE=${config.hsPkgs.cardano-node.components.exes.cardano-node}/bin/cardano-node${pkgs.stdenv.hostPlatform.extensions.executable}
        export CARDANO_NODE_SRC=${flakeopts.repoRoot}
      ";

      # FIXME: Haddock mysteriously gives a spurious missing-home-modules warning
      plutus-tx-plugin.doHaddock = false;

      # Relies on cabal-doctest, just turn it off in the Nix build
      prettyprinter-configurable.components.tests.prettyprinter-configurable-doctest.buildable = lib.mkForce false;

      e2e-tests.ghcOptions = [ "-Werror" ];

      # Honestly not sure why we need this, it has a mysterious unused dependency on "m"
      # This will go away when we upgrade nixpkgs and things use ieee754 anyway.
      ieee.components.library.libs = lib.mkForce [ ];

      # See https://github.com/input-output-hk/iohk-nix/pull/488
      cardano-crypto-praos.components.library.pkgconfig = lib.mkForce [ [ pkgs.libsodium-vrf ] ];
      cardano-crypto-class.components.library.pkgconfig = lib.mkForce [ [ pkgs.libsodium-vrf pkgs.secp256k1 ] ];
    };
  };

  # TODO this is temporary and will be done automatically by IOGX in the next version
  cardano-node-gitrev = "6b601db8c7fcd3b55e78244c21aca2d17ca3aeaa";


  project = pkgs.haskell-nix.cabalProject' (_: {

    compiler-nix-name = ghc;

    src = flakeopts.repoRoot;

    # Reccomended, otherwise you'll have to build haddock for the entire haskell
    # dependecy tree.
    shell.withHoogle = false;

    inputMap = {
      "https://input-output-hk.github.io/cardano-haskell-packages" = inputs.CHaP;
    };

    sha256map = {
      "https://github.com/input-output-hk/cardano-node".${cardano-node-gitrev} = "sha256-X8iZZMs0LepgTf1Mjh2mW/r5ttlVDSSjQ8GrrxMkHWY=";
      "https://github.com/james-iohk/cardano-node"."4ef9c0cba628307c70b24206d28b0a44fc9162bd" = "sha256-nhnYN49K2aS06MWmTxmzwnAcqINSA4yJsSTI7y9gRlI=";
      "https://github.com/input-output-hk/cardano-api"."89fd11781d8ba19ce50f516ecef30607d2e704e8" = "sha256-jiPYLSV8/c/Qu44ekYlM6LT7cuGt0Tr8wVgh3GYQSdg=";
    };

    # Configuration settings needed for cabal configure to work when cross compiling
    # for windows. We can't use `modules` for these as `modules` are only applied
    # after cabal has been configured.
    cabalProjectLocal = lib.optionalString pkgs.stdenv.hostPlatform.isWindows ''
      -- When cross compiling for windows we don't have a `ghc` package, so use
      -- the `plutus-ghc-stub` package instead.
      package plutus-tx-plugin
        flags: +use-ghc-stub

      -- Exlcude test that use `doctest`.  They will not work for windows
      -- cross compilation and `cabal` will not be able to make a plan.
      package prettyprinter-configurable
        tests: False
    '';

    modules = [ antaeus-module ] ++ pkgs.lib.optional enableProfiling { enableProfiling = true; };
  });
in
project.appendOverlays [
  (final: prev: {
    hsPkgs = pkgs.pkgsBuildBuild.setGitRevForPaths cardano-node-gitrev [
      "cardano-cli.components.exes.cardano-cli"
      "cardano-node.components.exes.cardano-node"
    ]
      prev.hsPkgs;
  })
]
