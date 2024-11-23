{
  description = "A Collection of Effectful Effects";

  # nix
  inputs.flake-parts.url = "github:hercules-ci/flake-parts";
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  inputs.nix-hs-utils.url = "github:tbidne/nix-hs-utils";

  # haskell
  inputs.algebra-simple = {
    url = "github:tbidne/algebra-simple";
    inputs.flake-parts.follows = "flake-parts";
    inputs.nixpkgs.follows = "nixpkgs";
    inputs.nix-hs-utils.follows = "nix-hs-utils";
  };
  inputs.bounds = {
    url = "github:tbidne/bounds";
    inputs.flake-parts.follows = "flake-parts";
    inputs.nixpkgs.follows = "nixpkgs";
    inputs.nix-hs-utils.follows = "nix-hs-utils";
  };
  inputs.exception-utils = {
    url = "github:tbidne/exception-utils";
    inputs.flake-parts.follows = "flake-parts";
    inputs.nixpkgs.follows = "nixpkgs";
    inputs.nix-hs-utils.follows = "nix-hs-utils";
  };
  inputs.fs-utils = {
    url = "github:tbidne/fs-utils";
    inputs.flake-parts.follows = "flake-parts";
    inputs.nixpkgs.follows = "nixpkgs";
    inputs.nix-hs-utils.follows = "nix-hs-utils";
  };
  outputs =
    inputs@{
      flake-parts,
      nix-hs-utils,
      self,
      ...
    }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      perSystem =
        { pkgs, ... }:
        let
          ghc-version = "ghc982";
          hlib = pkgs.haskell.lib;
          compiler = pkgs.haskell.packages."${ghc-version}".override {
            overrides =
              final: prev:
              {
                effectful-core = (
                  final.callHackageDirect {
                    pkg = "effectful-core";
                    ver = "2.5.0.0";
                    sha256 = "sha256-UCbMP8BfNfdIRTLzB4nBr17jxRp5Qmw3sTuORO06Npg=";
                  } { }
                );
                effectful = (
                  final.callHackageDirect {
                    pkg = "effectful";
                    ver = "2.5.0.0";
                    sha256 = "sha256-lmM0kdM5PS45Jol5Y2Nw30VWWfDPiPJLrwVj+GmJSOQ=";
                  } { }
                );
                strict-mutable-base = (
                  final.callHackageDirect {
                    pkg = "strict-mutable-base";
                    ver = "1.1.0.0";
                    sha256 = "sha256-cBSwoNGU/GZDW3eg7GI28t0HrrrxMW9hRapoOL2zU7Q=";
                  } { }
                );
              }
              // nix-hs-utils.mkLibs inputs final [
                "algebra-simple"
                "bounds"
                "exception-utils"
                "fs-utils"
              ];
          };
          pkgsCompiler = {
            inherit pkgs compiler;
          };
          pkgsMkDrv = {
            inherit pkgs;
            mkDrv = false;
          };
          hsOverlay = (
            compiler.extend (
              hlib.compose.packageSourceOverrides {
                environment-effectful = ./lib/environment-effectful;
                fs-effectful = ./lib/fs-effectful;
                ioref-effectful = ./lib/ioref-effectful;
                logger-effectful = ./lib/logger-effectful;
                logger-ns-effectful = ./lib/logger-ns-effectful;
                optparse-effectful = ./lib/optparse-effectful;
                stm-effectful = ./lib/stm-effectful;
                terminal-effectful = ./lib/terminal-effectful;
                concurrent-effectful = ./lib/concurrent-effectful;
                time-effectful = ./lib/time-effectful;
                typed-process-dynamic-effectful = ./lib/typed-process-dynamic-effectful;
                unix-compat-effectful = ./lib/unix-compat-effectful;
                unix-effectful = ./lib/unix-effectful;
              }
            )
          );
          packages = p: [
            p.environment-effectful
            p.fs-effectful
            p.ioref-effectful
            p.logger-effectful
            p.logger-ns-effectful
            p.optparse-effectful
            p.stm-effectful
            p.terminal-effectful
            p.concurrent-effectful
            p.time-effectful
            p.typed-process-dynamic-effectful
            p.unix-compat-effectful
            p.unix-effectful
          ];

          mkPkg =
            name: root: source-overrides:
            compiler.developPackage {
              inherit name root source-overrides;
              returnShellEnv = false;
            };
          compilerPkgs = {
            inherit compiler pkgs;
          };
        in
        {
          packages.concurrent-effectful = mkPkg "concurrent-effectful" ./lib/concurrent-effectful { };
          packages.env-guard-effectful = mkPkg "env-guard-effectful" ./lib/env-guard-effectful { };
          packages.environment-effectful = mkPkg "environment-effectful" ./lib/environment-effectful { };
          packages.fs-effectful = mkPkg "fs-effectful" ./lib/fs-effectful {
            ioref-effectful = ./lib/ioref-effectful;
            unix-compat-effectful = ./lib/unix-compat-effectful;
          };
          packages.ioref-effectful = mkPkg "ioref-effectful" ./lib/ioref-effectful { };
          packages.logger-effectful = mkPkg "logger-effectful" ./lib/logger-effectful { };
          packages.logger-ns-effectful = mkPkg "logger-ns-effectful" ./lib/logger-ns-effectful {
            concurrent-effectful = ./lib/concurrent-effectful;
            logger-effectful = ./lib/logger-effectful;
            time-effectful = ./lib/time-effectful;
          };
          packages.optparse-effectful = mkPkg "optparse-effectful" ./lib/optparse-effectful {
            fs-effectful = ./lib/fs-effectful;
            ioref-effectful = ./lib/ioref-effectful;
            unix-compat-effectful = ./lib/unix-compat-effectful;
          };
          packages.stm-effectful = mkPkg "stm-effectful" ./lib/stm-effectful { };
          packages.terminal-effectful = mkPkg "terminal-effectful" ./lib/terminal-effectful { };
          packages.time-effectful = mkPkg "time-effectful" ./lib/time-effectful { };
          packages.typed-process-dynamic-effectful =
            mkPkg "time-effectful" ./lib/typed-process-dynamic-effectful
              { };
          packages.unix-compat-effectful = mkPkg "unix-compat-effectful" ./lib/unix-compat-effectful { };
          packages.unix-effectful = mkPkg "unix-effectful" ./lib/unix-effectful { };

          devShells.default = hsOverlay.shellFor {
            inherit packages;
            withHoogle = true;
            buildInputs = (nix-hs-utils.mkBuildTools pkgsCompiler) ++ (nix-hs-utils.mkDevTools pkgsCompiler);
          };

          apps = {
            format = nix-hs-utils.mergeApps {
              apps = [
                (nix-hs-utils.format (pkgsCompiler // pkgsMkDrv))
                (nix-hs-utils.format-yaml pkgsMkDrv)
              ];
            };

            lint = nix-hs-utils.mergeApps {
              apps = [
                (nix-hs-utils.lint (pkgsCompiler // pkgsMkDrv))
                (nix-hs-utils.lint-yaml pkgsMkDrv)
              ];
            };

            lint-refactor = nix-hs-utils.lint-refactor pkgsCompiler;
          };
        };
      systems = [
        "aarch64-darwin"
        "aarch64-linux"
        "x86_64-darwin"
        "x86_64-linux"
      ];
    };
}
