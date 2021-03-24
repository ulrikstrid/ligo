# SPDX-FileCopyrightText: 2020 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

{ haskell-nix, grammars }:
let
  project = haskell-nix.stackProject {
    src = haskell-nix.haskellLib.cleanGit { src = ./.; };
    modules = [
      ({ config, ... }: {
        packages.ligo-squirrel = {
          preBuild = ''
            rm -rf grammar
            cp -r ${grammars} grammar
          '';
          # Thanks, I Hate It.
          components.tests.squirrel-test = {
            preBuild = "export CONTRACTS_DIR=${../../../src/test/contracts}";
          };

          package.ghcOptions = "-Werror";
        };
      })
    ];
  };
in project.ligo-squirrel