{ ocamlVersion ? "4_10" }:
let
  overlays =
    builtins.fetchTarball
      https://github.com/anmonteiro/nix-overlays/archive/2e77f2d29090cbcd21e526275cca22f525d0bbb2.tar.gz;

in
import "${overlays}/sources.nix" {
  overlays = [
    (import overlays)
    (self: super: {
      ocamlPackages =
        (super.ocaml-ng."ocamlPackages_${ocamlVersion}");

      pkgsCross.musl64.pkgsStatic = super.pkgsCross.musl64.pkgsStatic.appendOverlays [
        (self: super: {
          ocamlPackages = super.ocaml-ng."ocamlPackages_${ocamlVersion}";
        })
      ];
    })
  ];
}
