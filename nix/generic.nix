{ pkgs, stdenv, lib, ocamlPackages, static ? false, doCheck }:

with ocamlPackages;

let 
  version = "8.3";
  tezos_src = builtins.fetchurl {
    url = https://gitlab.com/tezos/tezos/-/archive/v8.3/tezos-v8.3.tar.bz2;
    sha256 = "17ca44fz5qx2l2w45fa5xkriwl95rzqai9l6rr16bllvr8j8dy8k";
  };
  tezos_preBuild = ''
    rm -rf vendors
  '';
  json_data_encoding_src = builtins.fetchurl {
    url = https://gitlab.com/nomadic-labs/json-data-encoding/-/archive/v0.8/json-data-encoding-v0.8.tar.gz;
    sha256 = "1mvzbhmrn66h44wcvwn6bgzssfcbdl6x7rmds31mngn2zs9h551z";
  };
in

rec {
  ligo = buildDunePackage {
    pname = "ligo";
    version = version;

    
    src = lib.filterGitSource {
      src = ./..;
      dirs = [ "src" "vendors" ];
    };

    useDune2 = true;

    propagatedBuildInputs = [
      pkgs.coq
      menhir
      ocamlgraph
      ppx_deriving
      ppx_deriving_yojson
      ppx_expect
      tezos-base
      tezos-shell-services
      tezos-protocol-008-PtEdo2Zk-parameters
      tezos-protocol-008-PtEdo2Zk
      tezos-protocol-environment
      tezos-shell-services
      yojson
      getopt
      terminal_size
      pprint
      linenoise
      data-encoding
      bisect_ppx

      # vendors
      ff
      alcotest-lwt
    ];

    checkInputs = [
    ];

    inherit doCheck;
  };

  tezos-shell-services = buildDunePackage {
    pname = "tezos-shell-services";
    version = version;

    src = tezos_src;
    useDune2 = true;

    preBuild = ''
      rm -rf vendors
    '';

    propagatedBuildInputs = [
      tezos-workers
      tezos-p2p-services
      tezos-version
    ];

    checkInputs = [
    ];

    inherit doCheck;
  };

  tezos-protocol-008-PtEdo2Zk-parameters = buildDunePackage {
    pname = "tezos-protocol-008-PtEdo2Zk-parameters";
    version = version;

    src = tezos_src;
    useDune2 = true;

    preBuild = ''
      rm -rf vendors
    '';

    propagatedBuildInputs = [
      tezos-protocol-008-PtEdo2Zk
    ];

    checkInputs = [
      lib-test
      qcheck-alcotest
    ];

    inherit doCheck;
  };

  tezos-protocol-008-PtEdo2Zk = buildDunePackage {
    pname = "tezos-protocol-008-PtEdo2Zk";
    version = version;

    src = tezos_src;
    useDune2 = true;

    preBuild = ''
      rm -rf vendors
      sed -i.back -e s/-nostdlib//g src/proto_008_PtEdo2Zk/lib_protocol/dune.inc
    '';

    propagatedBuildInputs = [
      tezos-protocol-compiler
    ];

    checkInputs = [
    ];

    inherit doCheck;
  };

  tezos-protocol-compiler = buildDunePackage {
    pname = "tezos-protocol-compiler";
    version = version;

    src = tezos_src;
    useDune2 = true;

    preBuild = ''
      rm -rf vendors
    '';

    propagatedBuildInputs = [
      tezos-protocol-environment
      ocp-ocamlres
      pprint
    ];

    checkInputs = [
    ];

    inherit doCheck;
  };

  tezos-protocol-environment = buildDunePackage {
    pname = "tezos-protocol-environment";
    version = version;

    src = tezos_src;
    useDune2 = true;

    preBuild = ''
      rm -rf vendors
    '';

    propagatedBuildInputs = [
      tezos-sapling
      tezos-base
      tezos-protocol-environment-sigs
      tezos-protocol-environment-structs
      zarith
    ];

    checkInputs = [
      alcotest-lwt
      crowbar
    ];

    inherit doCheck;
  };

  tezos-protocol-environment-sigs = buildDunePackage {
    pname = "tezos-protocol-environment-sigs";
    version = version;

    src = tezos_src;
    useDune2 = true;

    preBuild = ''
      rm -rf vendors
    '';

    propagatedBuildInputs = [
      tezos-stdlib
      tezos-protocol-environment-packer
    ];

    checkInputs = [
    ];

    inherit doCheck;
  };

  tezos-protocol-environment-structs = buildDunePackage {
    pname = "tezos-protocol-environment-structs";
    version = version;

    src = tezos_src;
    useDune2 = true;

    preBuild = ''
      rm -rf vendors
    '';

    propagatedBuildInputs = [
      tezos-crypto
      tezos-protocol-environment-packer
    ];

    checkInputs = [
    ];

    inherit doCheck;
  };

  tezos-protocol-environment-packer = buildDunePackage {
    pname = "tezos-protocol-environment-packer";
    version = version;

    src = tezos_src;
    useDune2 = true;

    preBuild = ''
      rm -rf vendors
    '';

    propagatedBuildInputs = [
    ];

    checkInputs = [
    ];

    inherit doCheck;
  };

  tezos-workers = buildDunePackage {
    pname = "tezos-workers";
    version = version;

    src = tezos_src;
    useDune2 = true;

    preBuild = ''
      rm -rf vendors
    '';

    propagatedBuildInputs = [
      tezos-base
    ];

    checkInputs = [
    ];

    inherit doCheck;
  };

  tezos-p2p-services = buildDunePackage {
    pname = "tezos-p2p-services";
    version = version;

    src = tezos_src;
    useDune2 = true;

    preBuild = ''
      rm -rf vendors
    '';

    propagatedBuildInputs = [
      tezos-base
    ];

    checkInputs = [
    ];

    inherit doCheck;
  };

  tezos-version = buildDunePackage {
    pname = "tezos-version";
    version = version;

    src = tezos_src;
    useDune2 = true;

    preBuild = ''
      rm -rf vendors
    '';

    propagatedBuildInputs = [
      tezos-base
    ];

    checkInputs = [
    ];

    inherit doCheck;
  };

  tezos-sapling = buildDunePackage {
    pname = "tezos-sapling";
    version = version;

    src = tezos_src;
    useDune2 = true;

    preBuild = ''
      rm -rf vendors
    '';

    propagatedBuildInputs = [
      tezos-crypto
      tezos-rust-libs
    ];

    checkInputs = [
      alcotest-lwt
    ];

    inherit doCheck;

    OPAM_SWITCH_PREFIX = "${ocamlPackages.tezos-rust-libs}/lib/ocaml/${ocamlPackages.ocaml.version}/site-lib";
  };

  tezos-base = buildDunePackage {
    pname = "tezos-base";
    version = version;

    src = tezos_src;
    useDune2 = true;

    preBuild = ''
      rm -rf vendors
    '';

    propagatedBuildInputs = [
      tezos-crypto
      tezos-micheline
      ptime
      ezjsonm
      ipaddr
    ];

    checkInputs = [
      lib-test
      qcheck-alcotest
    ];

    inherit doCheck;
  };

  tezos-crypto = buildDunePackage {
    pname = "tezos-crypto";
    version = version;

    src = tezos_src;
    useDune2 = true;

    preBuild = ''
      rm -rf vendors
    '';

    propagatedBuildInputs = [
      tezos-clic
      tezos-rpc
      bls12-381
      hacl-star
      secp256k1-internal
      uecc
      ringo
      ff
    ];

    checkInputs = [
      alcotest
      alcotest-lwt
    ];

    inherit doCheck;
  };

  tezos-micheline = buildDunePackage {
    pname = "tezos-micheline";
    version = version;

    src = tezos_src;
    useDune2 = true;

    preBuild = ''
      rm -rf vendors
    '';

    propagatedBuildInputs = [
      tezos-error-monad
      uutf
    ];

    checkInputs = [
      alcotest
      alcotest-lwt
    ];

    inherit doCheck;
  };

  tezos-clic = buildDunePackage {
    pname = "tezos-clic";
    version = version;

    src = tezos_src;
    useDune2 = true;

    preBuild = ''
      rm -rf vendors
    '';

    propagatedBuildInputs = [
      tezos-stdlib-unix
    ];

    checkInputs = [
      alcotest
      alcotest-lwt
    ];

    inherit doCheck;
  };

  tezos-stdlib-unix = buildDunePackage {
    pname = "tezos-stdlib-unix";
    version = version;

    src = tezos_src;
    useDune2 = true;

    preBuild = ''
      rm -rf vendors
    '';

    propagatedBuildInputs = [
      tezos-event-logging
      lwt
      ptime
      mtime
      ipaddr
      re
    ];

    checkInputs = [
      alcotest
      alcotest-lwt
    ];

    inherit doCheck;
  };

  tezos-event-logging = buildDunePackage {
    pname = "tezos-event-logging";
    version = version;

    src = tezos_src;
    useDune2 = true;

    preBuild = ''
      rm -rf vendors
    '';

    propagatedBuildInputs = [
      tezos-lwt-result-stdlib
      lwt_log
    ];

    checkInputs = [
      alcotest
      alcotest-lwt
    ];

    inherit doCheck;
  };

  tezos-lwt-result-stdlib = buildDunePackage {
    pname = "tezos-lwt-result-stdlib";
    version = version;

    src = tezos_src;
    useDune2 = true;

    preBuild = ''
      rm -rf vendors
    '';

    propagatedBuildInputs = [
      tezos-error-monad
    ];

    checkInputs = [
      alcotest
      alcotest-lwt
      crowbar
    ];

    inherit doCheck;
  };

  tezos-error-monad = buildDunePackage {
    pname = "tezos-error-monad";
    version = version;

    src = tezos_src;
    useDune2 = true;

    preBuild = ''
      rm -rf vendors
    '';

    propagatedBuildInputs = [
      tezos-stdlib
      data-encoding
      lwt
      lwt-canceler
    ];

    checkInputs = [
      alcotest
      alcotest-lwt
    ];

    inherit doCheck;
  };

  tezos-stdlib = buildDunePackage {
    pname = "tezos-stdlib";
    version = version;

    src = tezos_src;
    useDune2 = true;

    preBuild = ''
      rm -rf vendors
    '';

    propagatedBuildInputs = [
      hex
      lwt
      zarith
    ];

    checkInputs = [
      alcotest
      alcotest-lwt
      crowbar
      bigstring
      lwt_log
    ];

    inherit doCheck;
  };

  tezos-rpc = buildDunePackage {
    pname = "tezos-rpc";
    version = version;

    src = tezos_src;
    useDune2 = true;

    preBuild = ''
      rm -rf vendors
    '';

    propagatedBuildInputs = [
      tezos-error-monad
      resto
      resto-directory
    ];

    checkInputs = [
    ];

    inherit doCheck;
  };

  uecc = buildDunePackage {
    pname = "uecc";
    version = "0.3";

    src = builtins.fetchurl {
      url = https://gitlab.com/nomadic-labs/ocaml-uecc/-/archive/v0.3/ocaml-uecc-v0.3.tar.gz;
      sha256 = "0vcmgx98qd7c3yq4jq9f8qzvbir00z237ym7y2bq0s82vmdyhjxf";
    };
    useDune2 = true;

    propagatedBuildInputs = [
      bigstring
    ];

    checkInputs = [
      alcotest
      cstruct
      hex
    ];

    inherit doCheck;
  };

  lwt-canceler = buildDunePackage {
    pname = "lwt-canceler";
    version = "0.2";

    src = builtins.fetchurl {
      url = https://gitlab.com/nomadic-labs/lwt-canceler/-/archive/v0.2/lwt-canceler-v0.2.tar.gz;
      sha256 = "1n47wlnxp6macz94c1522y0gkxl4cdxcijfanh1xl2wkdbmkrvdf";
    };
    useDune2 = true;

    propagatedBuildInputs = [
      lwt
    ];

    checkInputs = [
    ];

    inherit doCheck;
  };

  ff = buildDunePackage {
    pname = "ff";
    version = "0.4.0";

    src = builtins.fetchurl {
      url = https://gitlab.com/dannywillems/ocaml-ff/-/archive/0.4.0/ocaml-ff-0.4.0.tar.gz;
      sha256 = "0mw12z24zyvs2dxwnh3d4a5ilp8dhlybgmpv6b26xbibp4i1lj5d";
    };
    useDune2 = true;

    propagatedBuildInputs = [
      zarith
    ];

    checkInputs = [
      alcotest
    ];

    inherit doCheck;
  };

  json-data-encoding = buildDunePackage {
    pname = "json-data-encoding";
    version = "0.8";

    src = json_data_encoding_src;
    useDune2 = true;

    propagatedBuildInputs = [
      uri
    ];

    checkInputs = [
      crowbar
    ];

    inherit doCheck;
  };

  json-data-encoding-bson = buildDunePackage {
    pname = "json-data-encoding-bson";
    version = "0.8";

    src = json_data_encoding_src;
    useDune2 = true;

    propagatedBuildInputs = [
      json-data-encoding
      ocplib-endian
    ];

    checkInputs = [
    ];

    inherit doCheck;
  };
  
  data-encoding = buildDunePackage {
    pname = "data-encoding";
    version = "0.2.0";

    src = builtins.fetchurl {
      url = https://gitlab.com/nomadic-labs/data-encoding/-/archive/0.2/data-encoding-0.2.tar.gz;
      sha256 = "1cb7rdx6rp837m6h6pp6268gcm9iaf3r8kl2qk6ffdmf3s4mwywz";
    };
    useDune2 = true;

    propagatedBuildInputs = [
      zarith
      ezjsonm
      hex
      json-data-encoding
      json-data-encoding-bson
    ];

    checkInputs = [
      alcotest
      crowbar
    ];

    inherit doCheck;
  };
  
  getopt = buildDunePackage {
    pname = "getopt";
    version = "20210705-dev";

    src = builtins.fetchurl {
      url = https://github.com/ulrikstrid/ocaml-getopt/archive/645ab740c5a8d2ca1ec1099e4a05580009a913e9.tar.gz;
      sha256 = "1qpnfv5wrh7kzi996fsfw62vl62gh45h3y6b3v71ahbqbxd1akgg";
    };
    useDune2 = true;

    propagatedBuildInputs = [
    ];

    checkInputs = [
    ];

    inherit doCheck;
  };

  bls12-381 = buildDunePackage {
    pname = "bls12-381";
    version = "0.3.15";

    src = builtins.fetchurl {
      url = https://gitlab.com/dannywillems/ocaml-bls12-381/-/archive/0.3.15/ocaml-bls12-381-0.3.15.tar.gz;
      sha256 = "0c0h47yy2viysip84w7afki4s0dwp1l2lskq64w85552jj8i43m1";
    };
    useDune2 = true;

    propagatedBuildInputs = [
      ff
      dune-configurator
      zarith
      ctypes
      tezos-rust-libs
      pkgs.rustc
      pkgs.cargo
    ];

    OPAM_SWITCH_PREFIX = "${ocamlPackages.tezos-rust-libs}/lib/ocaml/${ocamlPackages.ocaml.version}/site-lib";

    inherit doCheck;
  };

  tezos-rust-libs = buildDunePackage {
    pname = "tezos-rust-libs";
    version = "1.0";

    src = builtins.fetchurl {
      url = https://gitlab.com/tezos/tezos-rust-libs/-/archive/v1.0/tezos-rust-libs-v1.0.tar.bz2;
      sha256 = "1gfp62qvjcwgr3377vkdi6x9drpis2mz74ypx1bzsnxbm1jizj7j";
    };

    buildInputs = with ocamlPackages; [
      topkg
      findlib
    ];

    propagatedBuildInputs = [ pkgs.cargo pkgs.rustc ];

    buildPhase = ''
      mkdir .cargo
      mv cargo-config .cargo/config
      cargo build --target-dir target --release
    '';

    installPhase = ''
      ${pkgs.opaline}/bin/opaline -prefix $out -libdir $OCAMLFIND_DESTDIR/lib -name tezos-rust-libs
    '';

    inherit doCheck;

    meta = {
      description = "Tezos: all rust dependencies and their dependencies";
      license = lib.licenses.mit;
    };

    createFindlibDestdir = true;
  };
}
