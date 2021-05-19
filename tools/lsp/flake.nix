{
  inputs = {
    haskell-nix.url =
      "github:input-output-hk/haskell.nix/bd45da822d2dccdbb3f65d0b52dd2a91fd65ca4e";
    hackage-nix = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };
    stackage-nix = {
      url = "github:input-output-hk/stackage.nix";
      flake = false;
    };
    nix-npm-buildpackage.url = "github:serokell/nix-npm-buildpackage";
    nixpkgs.url = "github:serokell/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    { self, haskell-nix, hackage-nix, stackage-nix, nix-npm-buildpackage, flake-utils, nixpkgs }@inputs:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
      let
        haskellNix = import haskell-nix {
          sourcesOverride = { hackage = hackage-nix; stackage = stackage-nix; } // haskell-nix.sources;
        };

        nixpkgsArgs = {
          overlays = [
            nix-npm-buildpackage.overlay
            haskellNix.allOverlays.combined-eval-on-build
          ] ++ [
            (final: prev:
              let
                tree-sitter-prebuilt-tarballs = {
                  x86_64-linux = final.fetchurl {
                    url =
                      "https://github.com/tree-sitter/tree-sitter/releases/download/0.16.9/tree-sitter-linux-x64.gz";
                    sha256 =
                      "0apcs1gg1p3b3psbk36ssvrfzvqrmhfqz7ajca1w6mdqn66ri9nc";
                  };
                  x86_64-darwin = final.fetchurl {
                    url =
                      "https://github.com/tree-sitter/tree-sitter/releases/download/0.16.9/tree-sitter-osx-x64.gz";
                    sha256 =
                      "19zpl9lzqyqgsbsfk16y4sigpp46g3i3xpz3k5hmzfdr2ryvb4wr";
                  };
                };
                tree-sitter-prebuilt = builtins.mapAttrs (system: tarball:
                  final.stdenv.mkDerivation {
                    name = "tree-sitter-prebuilt";
                    src = tarball;
                    phases = [ "unpackPhase" "fixupPhase" ];
                    unpackPhase =
                      "mkdir -p $out/bin; zcat $src > $out/bin/tree-sitter; chmod +x $out/bin/tree-sitter";
                    fixupPhase =
                      final.lib.optionalString (system != "x86_64-darwin")
                      "patchelf --set-interpreter ${final.stdenv.glibc}/lib/ld-linux-x86-64.so.2 $out/bin/tree-sitter";
                  }) tree-sitter-prebuilt-tarballs;
              in { tree-sitter = tree-sitter-prebuilt.${system}; })
            (_: _: {
              inherit grammars;
            }) # We don't want any overlays (static, cross, etc) applied to grammars
          ];
          localSystem = system;
        };

        pkgs = import haskell-nix.sources.nixpkgs nixpkgsArgs;

        grammars = import ./squirrel/grammar { inherit pkgs; };

        squirrel = pkgs.callPackage ./squirrel { };

        squirrel-sexp-test = pkgs.stdenv.mkDerivation {
          name = "squirrel-sexp-test";
          src = ./squirrel;
          buildInputs = [ pkgs.bats squirrel.components.exes.ligo-vet ];
          doCheck = true;
          phases = [ "unpackPhase" "checkPhase" ];
          checkPhase = ''
            bats ./scripts
            touch $out
          '';
        };

        squirrel-grammar-test = pkgs.stdenv.mkDerivation {
          name = "squirrel-grammar-test";
          HOME = "/tmp";
          src = "${grammars}";
          buildInputs = [ pkgs.tree-sitter ];
          doCheck = true;
          phases = [ "unpackPhase" "checkPhase" ];
          checkPhase =
            let testDialect = dialect: ''
                   cd ${dialect}
                   tree-sitter test
                   cd ..
                 '';
                 dialects = ["camligo" "reasonligo" "pascaligo"];
             in pkgs.lib.strings.concatStrings (map testDialect dialects)
                + "touch $out";
        };

        lint = pkgs.stdenv.mkDerivation {
          name = "lint";
          src = ./squirrel;
          buildInputs = [ pkgs.haskellPackages.hlint ];
          doCheck = true;
          phases = [ "unpackPhase" "checkPhase" ];
          checkPhase = ''
            bash scripts/lint.sh
            touch $out
          '';
        };

        pack = pkg:
          pkg.overrideAttrs (_: {
            postInstall = with pkgs; ''
              mkdir -p $out/lib
              cp ${gmp}/lib/* $out/lib
              chmod -R 777 $out/lib/
              install_name_tool -change ${gmp}/lib/libgmp.10.dylib @executable_path/../lib/libgmp.dylib $out/bin/ligo-squirrel
              install_name_tool -change ${libffi}/lib/libffi.7.dylib /usr/lib/libffi.dylib $out/bin/ligo-squirrel
              install_name_tool -change ${libiconv}/lib/libiconv.dylib /usr/lib/libiconv.dylib $out/bin/ligo-squirrel
              install_name_tool -change ${darwin.Libsystem}/lib/libSystem.B.dylib /usr/lib/libSystem.B.dylib $out/bin/ligo-squirrel
              install_name_tool -change ${darwin.Libsystem}/lib/libSystem.B.dylib /usr/lib/libSystem.B.dylib $out/lib/libgmp.dylib
            '';
          });

        squirrel-static = if system == "x86_64-darwin" then {
          components.exes.ligo-squirrel =
            pack squirrel.components.exes.ligo-squirrel;
        } else
          pkgs.pkgsCross.musl64.callPackage ./squirrel { };

        exes = builtins.mapAttrs
          (_: project: project.components.exes.ligo-squirrel) {
            inherit squirrel squirrel-static;
          };

        per-platform-dispatcher = pkgs.writeTextFile {
          name = "ligo-squirrel";
          text = ''
            #!/bin/sh
            "./bin/$(uname)/bin/ligo-squirrel" "$@"
          '';
          executable = true;
        };

        ligo-squirrel-combined = pkgs.linkFarm "ligo-squirrel-combined" [
          {
            name = "bin/ligo-squirrel";
            path = per-platform-dispatcher;
          }
          {
            name = "bin/Linux";
            path = self.packages.x86_64-linux.squirrel-static;
          }
          {
            name = "bin/Darwin";
            path = self.packages.x86_64-darwin.squirrel-static;
          }
        ];

        vscode-extension-native = pkgs.callPackage ./vscode-plugin {
          ligo-squirrel = exes.squirrel-static;
        };

        vscode-extension = pkgs.callPackage ./vscode-plugin {
          ligo-squirrel = ligo-squirrel-combined;
        };
      in {
        packages = exes // {
          inherit vscode-extension-native vscode-extension;
        };
        checks = {
          inherit squirrel-sexp-test;
          inherit squirrel-grammar-test;
          inherit (squirrel.checks) lsp-test;
          inherit (squirrel.checks) ligo-contracts-test;
          inherit lint;
        };
        defaultPackage = self.packages.${system}.vscode-extension-native;
        # For debug/development reasons only
        legacyPackages = pkgs;
        devShell = pkgs.mkShell rec {
          buildInputs = [ pkgs.tree-sitter pkgs.nodejs ];
        };
      }) // {
        # docker image with the language server
        lsp-docker-image = { creationDate ? "1970-01-01T00:00:01Z" }: self.legacyPackages.x86_64-linux.dockerTools.buildImage {
          name = "ligo-lsp";
          tag = "latest";
          created = creationDate;
          contents = self.packages.x86_64-linux.squirrel-static;
          config = {
            Entrypoint = [ "ligo-squirrel" ];
          };

          # language server needs /tmp directory
          extraCommands = ''
            mkdir -m 0777 ./tmp
          '';
        };

        # skopeo package used by CI
        skopeo = self.legacyPackages.x86_64-linux.skopeo;
      };
}
