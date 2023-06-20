{
  description = "";

  inputs = {

    # The following inputs are managed by iogx:
    #
    #   CHaP, flake-utils, haskell.nix, nixpkgs, hackage,
    #   iohk-nix, sphinxcontrib-haddock, pre-commit-hooks-nix,
    #   haskell-language-server, nosys, std, bitte-cells, tullia.
    #
    # They will be available in both systemized and desystemized flavours.
    # Do not re-add those inputs again here.
    # If you need to, you can override them like this instead:
    #
    #   iogx.inputs.hackage.url = "github:input-output-hk/hackage/my-branch"
    iogx.url = "github:input-output-hk/iogx";
    iogx.inputs.hackage.follows = "hackage";
    iogx.inputs.CHaP.follows = "CHaP";
    hackage = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };
    CHaP = {
      url = "github:input-output-hk/cardano-haskell-packages?ref=repo";
      flake = false;
    };

    # Other inputs can be defined as usual.
    # foobar.url = "github:foo/bar";

  };

  outputs = inputs: inputs.iogx.mkFlake {

    # Boilerplate: simply pass your unmodified inputs here.
    inherit inputs;

    # Trace debugging information in the `mkFlake` function.
    debug = true;

    # While migrating to IOGX, you might want to keep the old flake outputs
    # alongside the new ones. An easy way to do this is to prefix (nest)
    # each output group { packages, apps, devShells, <nonstandard>, ... }
    # with a custom name.
    # For example, if `flakeOutputsPrefix = "__foo__"` then the flake will
    # have outputs like these:
    #   outputs.devShells.x86_64-darwin.__foo__.baz
    #   outputs.nonstandard.x86_64-linux.__foo__.bar
    # A value of "" means: do not nest.
    flakeOutputsPrefix = "";

    # The root of the repository.
    # The path *must* contain the cabal.project file.
    repoRoot = ./.;

    # The nonempty list of supported systems.
    systems = [ "x86_64-linux" "x86_64-darwin" ];

    # The nonempty list of supported GHC versions.
    # Available versions are: ghc8107, ghc927
    haskellCompilers = [ "ghc927" ];

    # The default GHC compiler, it must be one of haskellCompilers above.
    # When running `nix develop` this is the compiler that will be available
    # in the shell.
    defaultHaskellCompiler = "ghc927";

    # The host system for cross-compiling on migwW64, usually x86_64-linux.
    # A value of null means: do not cross-compile.
    haskellCrossSystem = null;

    # A file evaluating to a haskell.nix project.
    # For documentation, refer to the file ./nix/haskell-project.nix
    # generated by the template.
    haskellProjectFile = ./nix/haskell-project.nix;

    # A file evaluating to system-dependent flake outputs.
    # For documentation, refer to the file ./nix/per-system-outputs.nix
    # generated by the template.
    # A value of null means: no custom outputs.
    perSystemOutputsFile = null;

    # Shell prompt i.e. the value of the `PS1` evnvar.
    # Not that because this is a nix string that will be embedded in a bash
    # string, you need to double-escape the left slashes:
    # Example:
    #   bash: "\n\[\033[1;32m\][nix-shell:\w]\$\[\033[0m\] "
    #   shellPrompt: "\n\\[\\033[1;32m\\][nix-shell:\\w]\\$\\[\\033[0m\\] "
    shellPrompt = "\n\\[\\033[1;32m\\][antaeus:\\w]\\$\\[\\033[0m\\] ";

    # Only for cosmetic purposes, the shell welcome message will be printed
    # when you enter the shell.
    shellWelcomeMessage = "🤟 \\033[1;31mWelcome to antaeus\\033[0m 🤟";

    # A file evaluating to your development shell.
    # For documentation, refer to the file ./nix/shell-module.nix
    # generated by the template.
    # A value of null means: no need to augment the default shell.
    shellModuleFile = ./nix/shell-module.nix;

    # Whether to populate `hydraJobs` with the haskell artifacts.
    # In general you want to set this to true.
    # If this field is set to false, then the following fields have no
    # effect:
    #   excludeProfiledHaskellFromHydraJobs
    #   blacklistedHydraJobs
    #   enableHydraPreCommitCheck
    includeHydraJobs = true;

    # Whether to exclude profiled haskell builds from CI.
    # In general you don't want to run profiled builds in CI.
    excludeProfiledHaskellFromHydraJobs = true;

    # A list of derivations to be excluded from CI.
    # Each item in the list is an attribute path inside `hydraJobs` in the
    # form of a dot-string. For example:
    #   [ "packages.my-attrs.my-nested-attr.my-pkg" "checks.exclude-me" ]
    blacklistedHydraJobs = [ ];

    # Whether to run the pre-commit-check in CI, which mostly runs the
    # formatters. In general you want this to be true, but you can disable
    # it temporarily while migrating to IOGX if you find that the formatters
    # are producing large diffs on the source files.
    enableHydraPreCommitCheck = true;

    # The folder containing the read-the-docs python project.
    # You should set this value to something like:
    #   `readTheDocsSiteDir = ./doc/read-the-docs-site`
    # A value of null means: read-the-docs not available.
    # If this value is null, then the following fields have no effect:
    #   readTheDocsHaddockPrologue
    #   readTheDocsExtraHaddockPackages
    readTheDocsSiteDir = null;

    # A string to be appended to your haddock index page.
    # Haddock is included in the read-the-docs site.
    # A value of "" means: do not add a prologue.
    readTheDocsHaddockPrologue = "";

    # A function taking the project's haskell.nix package set and returning
    # a possibly empty attrset of extra haskell packages.
    # The haddock for the returned packages will be included in the final
    # haddock for this project.
    # The returned attrset must be of the form:
    #   `{ haskell-package-name: haskell-package }`
    # In general you want to include IOG-specific haskell dependencies here.
    # For example, in the haddock for plutus-apps you will want to include
    # the haddock for some plutus-core components, in which case you would
    # set this value like this:
    #   readTheDocsExtraHaddockPackages = hsPkgs: {
    #     inherit (hsPkgs)
    #       plutus-core plutus-tx plutus-tx-plugin
    #       plutus-ledger-api quickcheck-contractmodel
    #   }
    # A value of null means: do not add extra packages.
    readTheDocsExtraHaddockPackages = null;

    preCommitCheckHooks = { };
  };


  nixConfig = {

    # Do not remove these two substitures, but add to them if you wish.
    extra-substituters = [
      "https://cache.iog.io"
      "https://cache.zw3rk.com"
    ];

    # Do not remove these two public-keys, but add to them if you wish.
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
    ];

    accept-flake-config = true;

    # Do not remove this: it's needed by haskell.nix.
    allow-import-from-derivation = true;
  };
}
